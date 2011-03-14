
valkyree-gambit.o1: valkyree-gambit.scm valk-num.scm valk-snd.scm valk-seq.scm valk-file.scm valk-output.scm valk-riff.scm valk-extvec.scm
	$(HOME)/local/Gambit-C/bin/gsc -keep-c -dynamic -o valkyree-gambit.o1 valkyree-gambit.scm