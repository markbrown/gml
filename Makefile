PREFIX=$(HOME)/Sites
SRCDIRS=src src/compiler src/runtime src/runtime/render

install: doc
	mkdir -p $(PREFIX)/gml/doc
	cp -R doc/pdf $(PREFIX)/gml/doc
	cp -R doc/html $(PREFIX)/gml/doc
	cp -R src $(PREFIX)/gml
	for d in $(SRCDIRS); do \
		cd $(PREFIX)/gml/$$d && \
		rm -f index.html && \
		wget http://localhost/~mark/gml/$$d; \
	done
	cd $(PREFIX) && tar zcf gml.tgz gml

.PHONY: doc
doc:
	cd doc/src && $(MAKE) install

.PHONY: clean realclean siteclean
clean:
	cd doc/src && $(MAKE) clean

realclean:
	cd doc/src && $(MAKE) realclean

siteclean:
	cd $(PREFIX)/gml && rm -rf *

