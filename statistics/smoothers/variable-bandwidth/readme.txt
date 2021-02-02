This contains instructions on how to use the three *.lsp file in this
directory.

They are used to facilitate interactive bandwidth choice for estimator
(3.14), page 44 in Bagkavos (2003), "BIAS REDUCTION IN NONPARAMETRIC
HAZARD RATE ESTIMATION" Phd thesis, The University of Birmingham,
UK. Alternatively, as it may be difficult for potential users to
obtain the thesis, the file "VBHRE.pdf" which is attached in this
archive has the relevant chapter of the thesis. In this case it is
estimator (18), page 10 which is implemented through the three *.lsp
files.

Interaction with the system (and thus control of the estimators) is
driven by `dialogs' which are special forms of programming
objects. Their purpose is to provide an easy way for the user to
specify any parameters to be used.  Parameters of interest in our case
would be the kernel, the bandwidth, the distribution, the sample size
and the number of points at which the estimate is computed. Within a
dialog there are two ways for the user to choose the parameters
depending on the kind of parameters to be selected. If the user wants
to choose the sample size or the number of points at which the
estimate is evaluated then this is done by clicking the arrows of a
`slider'. The right arrow of the slider increases and the left arrow
decreases the value of the desired parameter. The value of the
parameter can be seen right next to the appropriate text field (sample
size or Eval. points). On the other hand if the user wants to specify
the distribution to be used or the kernel, this is done simply by
ticking the corresponding radio button from the list of available
distributions.

The process of selection of parameters is organized in two stages with
each stage being a dialog. At first stage the user specifies general
parameters, i.e. distribution, sample size, etc. The second stage is
selection of bandwidth values. This second dialog is present
throughout the estimation procedure so that the user can each time
choose a different pair of bandwidths. The output appears in a third
window after the user clicks `OK' for the first time. It has the true
hazard rate (if distributional data is used) and the estimate which
corresponds to the current bandwidth selection. Choosing new bandwidth
values and clicking OK the program draws the new estimate, overlayed
on the output window. Finally the procedure stops at any time simply
by clicking `Cancel' on the dialog of figure.

Sample session (assuming windows operating system and that xlisp-stat
is installed):

From the "File" menu, click:
1) File>load> VAR_BAND_DIALOG.LSP
2) File>load> VAR_BAND_HAZ_EST.LSP
3) File>load> VARBANDTEST1.LSP


type:

(var-band-dialog) and press <enter>

the dialog appears, promting to choose kernel and distribution. Choose
Epanechnikov and chi-square and click OK.  The plot window appears
together with the second dialog window prompting to choose bandwidths
for the adaptive and the pilot estimate.  Move the sliders to 0.2 for
both bandwidths and click "Start simulation". The estimate appears on
the plot window in red.  Move the slider of the adaptive to 0.4 (leave
the pilot bandwidth to 0.2). A second estimate appears on the plot
window in blue.  Click cancel. A summary window appears, containing
the bandwidths used and the colors of the estimates.

Please note that up to 7 runs are supported. After that the code
signals an error.


