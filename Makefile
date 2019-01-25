xmkit: xmkit.scm definitions.scm
	csc -s -O3 -d1 xmkit.scm -j xmkit
	csc -s xmkit.import.scm -O3 -d0

docs: xmkit.scm
	$(info $(shell mkdir -p docs && mkdir -p docs/html))
	scm2wiki -i xmkit.scm -o docs/xmkit.wiki
	manual-labor docs/ docs/html/

.PHONY: run-local-tests
run-local-tests: xmkit
	cp -t ./ tests/{run.scm,test.xm} && csi run.scm -e
	rm run.scm test.xm sample.raw

.PHONY: clean
clean:
	rm *.so *.import.scm
