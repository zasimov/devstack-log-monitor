all:: openstack_log_parser

openstack_log_parser::
	ghc --make Rsyslog.hs -o openstack_log_parser

clean::
	find . -name "*~" -delete
	find . -name "*.o" -delete
	find . -name "*.hi" -delete

