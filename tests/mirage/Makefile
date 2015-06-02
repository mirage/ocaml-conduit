all: http-server-unix http-fetch-unix vchan-client-xen vchan-server-xen
	@

http-server-unix:
	cd http-server && mirage configure --unix && make

http-fetch-unix:
	cd http-fetch && mirage configure --unix && make

vchan-client-xen:
	cd vchan && mirage configure config_client.ml --xen && make

vchan-server-xen:
	cd vchan && mirage configure config_server.ml --xen && make
