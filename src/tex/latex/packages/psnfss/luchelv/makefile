all: vpl vf

vpl:
	tex phvluc

vf:
	vptovf phvlucrm.vpl phvlucrm.vf phvlucrm.tfm
	vptovf phvlucbf.vpl phvlucbf.vf phvlucbf.tfm


clean:
	-rm *.vpl *.tfm *.vf *.aux *.log *.dvi *.fd *.pl hlc*.mtx phv*.mtx
