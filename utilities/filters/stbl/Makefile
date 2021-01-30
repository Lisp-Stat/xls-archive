all:    stbl.nw
	noweave -index -delay stbl.nw > stbl.tex
	notangle -R'Hyper-document' stbl.nw > hyperdoc.tex
	notangle -R'Utilities file' stbl.nw > utils.lsp
	notangle -R'Button overlay file' stbl.nw > button.lsp
	notangle -R'Distribution proto file' stbl.nw > distproto.lsp
	notangle -R'Distributions file' stbl.nw > dists.lsp
	notangle -R'Statistical tables file' stbl.nw > stbls.lsp
	notangle -R'Program hist.lsp' stbl.nw > hist.lsp
	notangle -R'Program smhist.lsp' stbl.nw > smhist.lsp
	notangle -R'Program 68.lsp' stbl.nw > 68.lsp
	notangle -R'Program 95.lsp' stbl.nw > 95.lsp
	notangle -R'Program 99.lsp' stbl.nw > 99.lsp
	notangle -R'Readme file' stbl.nw > README

