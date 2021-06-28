all:
#	service
	rm -f ebin/*;
	erlc -o ebin src/*.erl;
	rm -rf src/*.beam *.beam  test_src/*.beam test_ebin;
	rm -rf  *~ */*~  erl_cra*;
	rm -rf *_specs *_config *.log;
	rm -rf support etcd catalog cluster_config host_config;
	echo Done
doc_gen:
	echo glurk not implemented
unit_test:
	rm -rf ebin/* src/*.beam *.beam test_src/*.beam test_ebin;
	rm -rf  *~ */*~  erl_cra*;
	rm -rf *_specs *_config *.log;
#	support
	rm -rf support;
	git clone https://github.com/joq62/support.git;
#	etcd
	rm -rf etcd;
#	git clone https://github.com/joq62/etcd.git;
#	iaas
#	cp src/iaas.app ebin;
	erlc -o ebin src/*.erl;
#	test application
	mkdir test_ebin;
	cp test_src/*.app test_ebin;
	cp ../oam/src/*.erl test_src;
	erlc -o test_ebin test_src/*.erl;
	erl -pa ebin -pa test_ebin -pa support/ebin\
	    -setcookie abc\
	    -sname test_iaas\
	    -run unit_test start_test test_src/test.config
