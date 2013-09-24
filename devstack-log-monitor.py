#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gobject
import gtk
import pango


import json
import contextlib
import os
import sys
import threading
import time
import subprocess


@contextlib.contextmanager
def threads():
    gtk.gdk.threads_enter()
    try:
        yield
    finally:
        gtk.gdk.threads_leave()


class StoppableThread(threading.Thread):
    """Thread class with a stop() method. The thread itself has to check
    regularly for the stopped() condition."""

    def __init__(self):
        super(StoppableThread, self).__init__()
        self._stop = threading.Event()

    def stop(self):
        self._stop.set()

    def stopped(self):
        return self._stop.isSet()


class Tail(StoppableThread):
    ''' Represents a tail command. '''
    def __init__(self, service_log):
        ''' Initiate a Tail instance.
            Check for file validity, assigns callback function to standard out.
            Arguments:
                tailed_file - File to be followed. '''
        super(Tail, self).__init__()
        self.service_log = service_log
        self.check_file_validity(service_log.log_file)
        self.callback = self.service_log.parse_line

    def follow(self, s=0.1):
        ''' Do a tail follow. If a callback function is registered it
        is called with every new line.

        Else printed to standard out.

        Arguments:

          s - Number of seconds to wait between each iteration; Defaults
              to 1.'''

        with open(self.service_log.log_file) as file_:
            # Go to the end of file
            file_.seek(0,2)
            while not self.stopped():
                curr_position = file_.tell()
                line = file_.readline()
                if not line:
                    file_.seek(curr_position)
                    if s is not None:
                        time.sleep(s)
                else:
                    self.callback(line)

    def run(self):
        return self.follow()

    def register_callback(self, func):
        ''' Overrides default callback function to provided function. '''
        self.callback = func

    def check_file_validity(self, file_):
        ''' Check whether the a given file exists, readable and is a file '''
        if not os.access(file_, os.F_OK):
            raise TailError("File '%s' does not exist" % (file_))
        if not os.access(file_, os.R_OK):
            raise TailError("File '%s' not readable" % (file_))
        if os.path.isdir(file_):
            raise TailError("File '%s' is a directory" % (file_))


class TailError(Exception):
    def __init__(self, msg):
        self.message = msg
    def __str__(self):
        return self.message


class ProtectedVariable:
    def __init__(self):
        self.var = None
        self._lock = threading.Lock()

    def set(self, var):
        self._lock.acquire()
        try:
            self.var = var
        finally:
            self._lock.release()

    def get(self):
        self._lock.acquire()
        try:
            return self.var
        finally:
            self._lock.release()


class ServiceLog:
    MAX_CACHE_SIZE = 1

    COLORS = {'ERROR': '#FF0000'}

    def __init__(self, service_name, log_file, parser):
        self.service_name = service_name
        self.log_file = log_file
        self.parser = parser
        self.liststore = ProtectedVariable()
        self._cache = []

    def set_liststore(self, liststore):
        self.liststore.set(liststore)

    def get_color(self, entry):
        if 'level' in entry:
            level = entry['level']
            return self.COLORS.get(level, None)
        else:
            return None

    def push(self, liststore, log_entries):
        with threads():
            for entry in log_entries:
                if 'error' in entry:
                    liststore.append([entry.get('tag', 'unknown'),
                                      entry.get('remoteDate', 'unknown'),
                                      entry.get('level', 'unknown'),
                                      entry.get('component', 'unknown'),
                                      entry.get('entry', 'unknown'),
                                      entry.get('pid', 'unknown'),
                                      self.get_color(entry)])
                else:
                    liststore.append([entry.get('tag', 'unknown'),
                                      entry.get('remoteDate', 'unknown'),
                                      entry.get('level', 'unknown'),
                                      entry.get('component', 'unknown'),
                                      entry.get('message', 'unknown'),
                                      entry.get('pid', 'unknown'),
                                      self.get_color(entry)])

    def go(self):
        proc = subprocess.Popen(self.parser,
                                stdin=subprocess.PIPE,
                                stdout=subprocess.PIPE)
        proc.stdin.write("\n".join(self._cache))
        proc.stdin.close()
        data = proc.stdout.read()
        proc.stdout.close()
        liststore = self.liststore.get()
        if liststore is None:
            return
        self.push(liststore, json.loads(data))
        self._cache = []

    def parse_line(self, line):
        """ Callback from Tail. """
        self._cache.append(line)
        if len(self._cache) >= self.MAX_CACHE_SIZE:
            self.go()


class MainWindow:
    def delete_event(self, widget, event, data=None):
        # If you return FALSE in the "delete_event" signal handler,
        # GTK will emit the "destroy" signal. Returning TRUE means
        # you don't want the window to be destroyed.
        # This is useful for popping up 'are you sure you want to quit?'
        # type dialogs.

        # Change FALSE to TRUE and the main window will not be destroyed
        # with a "delete_event".
        return False

    def destroy(self, widget, data=None):
        for wather in self.wathers:
            wather.set_liststore(None)
        gtk.main_quit()

    def _autoscroll(self, *args):
        """The actual scrolling method"""
        if self._do_autoscroll:
            adj = self._sw.get_vadjustment()
            adj.set_value(adj.get_upper() - adj.get_page_size())

    def toggle_autoscroll(self, button):
        self._do_autoscroll = button.get_active()
        self._autoscroll()

    def filter(self, model, iter):
        service = model.get(iter, 0)[0]
        date = model.get(iter, 1)[0]
        component = model.get(iter, 3)[0]
        level = model.get(iter, 2)[0]
        message = model.get(iter, 4)[0]
        return (service in self.visible_services() and
                level in self.visible_levels())

    def visible_services(self):
        return map(lambda s: s[1],
                   filter(lambda s: s[0], self.services))# + ['unknown']

    def visible_levels(self):
        return map(lambda s: s[1],
                   filter(lambda s: s[0], self.levels))# + ['unknown']

    def refilter(self):
        self.modelfilter.refilter()

    def select_service(self, renderer, path, userdata):
        (model, column) = userdata
        model[path][column] = not model[path][column]
        self.refilter()

    def select_level(self, renderer, path, userdata):
        (model, column) = userdata
        model[path][column] = not model[path][column]
        self.refilter()

    def __init__(self, wathers):
        self._do_autoscroll = True

        self.wathers = wathers[:]

        self.liststore = gtk.ListStore(str, str, str, str, str, str, str)
        for wather in self.wathers:
            wather.set_liststore(self.liststore)

        # create a new window
        self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)
        self.window.set_size_request(800, 800)

        self.window.set_title('DevStack Log Monitor')

        # When the window is given the "delete_event" signal (this is given
        # by the window manager, usually by the "close" option, or on the
        # titlebar), we ask it to call the delete_event () function
        # as defined above. The data passed to the callback
        # function is NULL and is ignored in the callback function.
        self.window.connect("delete_event", self.delete_event)

        # Here we connect the "destroy" event to a signal handler.
        # This event occurs when we call gtk_widget_destroy() on the window,
        # or if we return FALSE in the "delete_event" callback.
        self.window.connect("destroy", self.destroy)

        scrolled_window = gtk.ScrolledWindow()
        scrolled_window.set_policy(
            gtk.POLICY_AUTOMATIC,
            gtk.POLICY_ALWAYS)
        self.modelfilter = self.liststore.filter_new(root=None)
        self.modelfilter.set_visible_func(self.filter)
        self.modelfilter.refilter()
        self.treeview = gtk.TreeView(self.modelfilter)
        self.treeview.connect('size-allocate', self._autoscroll)
        scrolled_window.add(self.treeview)
        self._sw = scrolled_window

        tvc = gtk.TreeViewColumn('service')
        text = gtk.CellRendererText()
        tvc.pack_start(text, True)
        tvc.add_attribute(text, 'text', 0)

        self.treeview.append_column(tvc)

        tvc = gtk.TreeViewColumn('PID')
        text = gtk.CellRendererText()
        tvc.pack_start(text, True)
        tvc.add_attribute(text, 'text', 5)

        self.treeview.append_column(tvc)

        tvc = gtk.TreeViewColumn('date')
        text = gtk.CellRendererText()
        tvc.pack_start(text, True)
        tvc.add_attribute(text, 'text', 1)

        self.treeview.append_column(tvc)

        tvc = gtk.TreeViewColumn('level')
        text = gtk.CellRendererText()
        text.set_property('foreground-set', True)
        tvc.pack_start(text, True)
        tvc.add_attribute(text, 'text', 2)
        tvc.add_attribute(text, 'foreground', 6)

        self.treeview.append_column(tvc)

        tvc = gtk.TreeViewColumn('component')
        text = gtk.CellRendererText()
        tvc.pack_start(text, True)
        tvc.add_attribute(text, 'text', 3)

        self.treeview.append_column(tvc)

        tvc = gtk.TreeViewColumn('message')
        text = gtk.CellRendererText()
        text.set_property('ellipsize', pango.ELLIPSIZE_END)
        tvc.pack_start(text, True)
        tvc.add_attribute(text, 'text', 4)

        self.treeview.append_column(tvc)

        # Sets the border width of the window.
        self.window.set_border_width(10)

        vbox = gtk.VBox()
        self.autoscroll = gtk.CheckButton('Autoscroll')
        self.autoscroll.set_active(True)
        self.autoscroll.connect("toggled", self.toggle_autoscroll)
        vbox.pack_start(self.autoscroll, expand=False, fill=False)

        vbox.pack_start(scrolled_window, expand=True, fill=True)

        services = gtk.ListStore(bool, str)
        services.append([True, 'cinder-api'])
        services.append([True, 'cinder-scheduler'])
        services.append([True, 'cinder-volume'])
        services.append([True, 'httpd'])
        services.append([True, 'nova-api'])
        services.append([True, 'nova-compute'])
        services.append([True, 'nova-conductor'])
        services.append([True, 'nova-scheduler'])
        self.services = services

        levels = gtk.ListStore(bool, str)
        levels.append([True, 'DEBUG'])
        levels.append([True, 'INFO'])
        levels.append([True, 'WARNING'])
        levels.append([True, 'ERROR'])
        levels.append([True, 'CRITICAL'])
        levels.append([True, 'AUDIT'])
        self.levels = levels

        control = gtk.VBox()

        # Services tree view
        treeview = gtk.TreeView(services)

        tvc = gtk.TreeViewColumn('show')
        toggle = gtk.CellRendererToggle()
        tvc.pack_start(toggle, False)
        toggle.set_property('activatable', True)
        tvc.add_attribute(toggle, 'active', 0)
        toggle.connect("toggled", self.select_service,
                       (self.services, 0))
        treeview.append_column(tvc)

        tvc = gtk.TreeViewColumn('service')
        text = gtk.CellRendererText()
        tvc.pack_start(text, True)
        tvc.add_attribute(text, 'text', 1)
        treeview.append_column(tvc)

        control.pack_start(treeview, expand=True, fill=True)

        # levels tree view
        treeview = gtk.TreeView(levels)

        tvc = gtk.TreeViewColumn('show')
        toggle = gtk.CellRendererToggle()
        tvc.pack_start(toggle, False)
        toggle.set_property('activatable', True)
        tvc.add_attribute(toggle, 'active', 0)
        toggle.connect("toggled", self.select_level,
                       (self.levels, 0))
        treeview.append_column(tvc)

        tvc = gtk.TreeViewColumn('level')
        text = gtk.CellRendererText()
        tvc.pack_start(text, True)
        tvc.add_attribute(text, 'text', 1)
        treeview.append_column(tvc)

        control.pack_start(treeview, expand=False, fill=False)

        hpaned = gtk.HPaned()
        hpaned.add1(control)
        hpaned.add2(vbox)
        hpaned.set_position(200)

        self.window.add(hpaned)

        # and the window
        self.window.show_all()

    def main(self):
        # All PyGTK applications must have a gtk.main(). Control ends here
        # and waits for an event to occur (like a key press or mouse event).
        gtk.main()


if __name__ == '__main__':
    gobject.threads_init()
    gtk.gdk.threads_init()

    tailers = []

    log = ServiceLog('all',
                     '/var/log/vm/devstack-grizzly',
                     './openstack_log_parser')
    tailer = Tail(log)
    tailer.start()
    tailers.append(tailer)

    window = MainWindow(wathers=[log])

    try:
        window.main()
    finally:
        for tailer in tailers:
            tailer.stop()
            tailer.join()
