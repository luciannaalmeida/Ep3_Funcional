COMPILE=scalac-2.8 -cp scalatest.jar


test : compile
	scala-2.8 -cp scalatest.jar org.scalatest.tools.Runner -p . -o -s pfc.PolSpec

compile : src/pfc/Pol.scala
	${COMPILE} src/pfc/Pol.scala        
 
clean : 
	rm -rf pfc
