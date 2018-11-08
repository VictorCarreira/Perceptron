all: fbench run

fbench:
	f2py3 -c fbench.f95 -m fbench > f2py_build.log 2> f2py_build.err

run: 
	python3 run_tests.py

clean:
	rm -rf *.pyc __pycache__ 

distclean:
	rm -rf *.so *.pyc __pycache__ *.log *.err *.png 
