all:
	rm -rf  *Mnesia erl_cra*;
	rm -rf  *~ */*~;
	rm -rf ebin/* test_ebin/* *.beam test_src/*.beam;
	rm -rf cluster* server common dbase;
#       iaas
	cp src/*.app ebin;
	erlc -o ebin src/*.erl
doc_gen:
	rm -rf  node_config logfiles doc/*;
	erlc ../doc_gen.erl;
	erl -s doc_gen start -sname doc
test:
	rm -rf  *Mnesia erl_cra*;
	rm -rf  *~ */*~;
	rm -rf ebin/* test_ebin/* *.beam test_src/*.beam;
	rm -rf cluster* server common dbase;
	erlc -o test_ebin test_src/*.erl;
#       iaas
	cp src/*.app ebin;
	erlc -o ebin src/*.erl;
	erl -pa test_ebin\
	    -pa ebin\
	    -s iaas_tests start -sname server -setcookie abc
