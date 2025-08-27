all: vpl vf

vpl:
	tex mntluc

vf:
	vptovf mntlucrm.vpl mntlucrm.vf mntlucrm.tfm
	vptovf mntlucbf.vpl mntlucbf.vf mntlucbf.tfm
	vptovf mntlucmi.vpl mntlucmi.vf mntlucmi.tfm
	vptovf mntlucmn.vpl mntlucmn.vf mntlucmn.tfm
	vptovf mntlucbn.vpl mntlucbn.vf mntlucbn.tfm


clean:
	-rm *.vpl *.tfm *.vf *.aux *.log *.dvi *.fd *.pl hlc*.mtx mnt*.mtx
