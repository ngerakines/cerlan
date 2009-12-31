.PHONY: templates

all: sources templates

sources: 
	mkdir -p ./ebin/
	erlc -o ./ebin/ src/*.erl

templates: 
	mkdir -p ./ebin/
	erl -noshell -eval "erltl:compile(\"./templates/cerlan_thome.et\", [{outdir, \"./ebin\"}, report_errors, report_warnings, nowarn_unused_vars])." -s init stop
	erl -noshell -eval "erltl:compile(\"./templates/cerlan_tuser.et\", [{outdir, \"./ebin\"}, report_errors, report_warnings, nowarn_unused_vars])." -s init stop
	erl -noshell -eval "erltl:compile(\"./templates/cerlan_textra.et\", [{outdir, \"./ebin\"}, report_errors, report_warnings, nowarn_unused_vars])." -s init stop
	erl -noshell -eval "erltl:compile(\"./templates/cerlan_tfaq.et\", [{outdir, \"./ebin\"}, report_errors, report_warnings, nowarn_unused_vars])." -s init stop

dev:
	erl -pa ./ebin ../heman/ ../heman/ebin -name httpdmaster1@`hostname` -boot start_sasl -setcookie supersecret -mnesia dump_log_write_threshold 50000 -mnesia dc_dump_limit 40

dev-server:
	/usr/local/lib/erlang/lib/mochevent-0.0.1/priv/mocheventcnode --ip 0.0.0.0 --port 8084 --master httpdmaster1@`hostname`
