#####################################################################
# Routine to create synthetic spectra of GCs from Usher+17 sample
#####################################################################
#---------------------------------
# Define functions
#---------------------------------
# For a description of the functions and parameters, see manual.pdf
source('stpars.R')
source('pfant12.R')
source('SSP_model.R')
source('useful.R')

#---------------------------------
# Set parameters
#---------------------------------
n_ms = 9         # number of desired main sequence stars
n_rg = 6         # number of desired red giant stars
feh = -1.0       # [Fe/H]
afe = 0.3        # [alpha/Fe]
CFe = -0.03          
NFe = 0.67         
OFe = 0.36
MgFe = 0.23
SiFe = 0.13
CaFe = 0.13
TiFe = 0.10
NaFe = -0.07        
CFe_rgb = -0.2      
NFe_rgb =  1.0     
BaFe = 0.02         
AlFe = -0.11         
EuFe = -0.14         
age = 11          # age (Gyr)
lmin = 4500       # lower lambda
lmax = 6800       # upper lambda
imf = 'salpeter'  # IMF

output_fig = "NGC6522_SSP.pdf"

compare = F                      # Compare 2 synthetic spectra?
legend_comp1 = 'VALD line list'  # Legend synthetic spectrum 1
legend_comp2 = 'atom4070g'       # Legend synthetic spectrum 2
filename_comp = '_4070g'         # File name of synthetic spectrum 2 (SEE LINE 83)

# ------------------------------------------------------------------------
## Select pairs of Teff, logg from Dartmouth isochrones
# ------------------------------------------------------------------------
stpars(n_ms, n_rg, feh, afe, age, logg_cn = 3, fig = T)

# ------------------------------------------------------------------------
## Now calling pfant to the desire wavelength interval lmin, lmax
# ------------------------------------------------------------------------
pfant12(feh, afe, lmin, lmax, age, fwhm = 0.12, n_ms = n_ms, n_rg = n_rg, CFe = CFe, NFe = NFe, OFe = OFe,
        TiFe = TiFe, SiFe = SiFe, MgFe = MgFe, CaFe = CaFe, NaFe = NaFe, BaFe = BaFe, AlFe = AlFe, EuFe = EuFe, 
        CFe_rgb = CFe_rgb, NFe_rgb = NFe_rgb)

# ------------------------------------------------------------------------
## Finally, adding the synthetic stellar and creating SSP
# ------------------------------------------------------------------------
ssp.model(feh, afe, age, imf, fwhm = 1.0, n_ms = n_ms, n_rg = n_rg, CFe = CFe, NFe = NFe, OFe = OFe,
          TiFe = TiFe, SiFe = SiFe, MgFe = MgFe, CaFe = CaFe, NaFe = NaFe, BaFe = BaFe, AlFe = AlFe, EuFe = EuFe, 
          CFe_rgb = CFe_rgb, NFe_rgb = NFe_rgb, lmin = lmin, lmax = lmax)

# ------------------------------------------------------------------------
## Read the SSP spectrum
# ------------------------------------------------------------------------
file_ssp <- set.ssp.filename(feh, afe, age = age, imf = imf, CFe, NFe = NFe, OFe = OFe,
                                  TiFe = TiFe, SiFe = SiFe, MgFe = MgFe, CaFe = CaFe, NaFe = NaFe, BaFe = BaFe, AlFe = AlFe, EuFe = EuFe, 
                                  CFe_rgb = CFe_rgb, NFe_rgb = NFe_rgb)
spec = read.table(file_ssp)
wave = spec$V1
flux = spec$V2

temp <- fit_cont(x = wave, y = flux)

mean_flux = predict(temp, newdata = data.frame(x1_temp = wave))
flux_norm = flux/mean_flux

# ------------------------------------------------------------------------
## Read spectrum for comparison
# ------------------------------------------------------------------------
if(compare == T){
  file_sspcomp = paste(file_ssp, filename_comp, sep = '')
  #file_sspcomp = set.ssp.filename(feh, afe, age = age, imf = imf, CFe, NFe = NFe, OFe = OFe,
  #                                 TiFe = TiFe, SiFe = SiFe, MgFe = MgFe, CaFe = CaFe, NaFe = NaFe, BaFe = BaFe, AlFe = AlFe, EuFe = EuFe, 
  #                                 CFe_rgb = -0.2, NFe_rgb = 1)
  speccomp = read.table(file_sspcomp)
  wavecomp = speccomp$V1
  fluxcomp = speccomp$V2
  
  temp <- fit_cont(x = wavecomp, y = fluxcomp)
  
  mean_fluxcomp = predict(temp, newdata = data.frame(x1_temp = wavecomp))
  flux_normcomp = fluxcomp/mean_fluxcomp
}

# ------------------------------------------------------------------------
# Read Usher+17 spectra
# ------------------------------------------------------------------------
u_band = read.table('DATA/NGC6522_U7000_2016-04-02.dat')
temp <- fit_cont(x = u_band$V1, y = u_band$V2, nsig_low = 1.5)
u_band_norm = u_band$V2/predict(temp, newdata = data.frame(x1_temp = u_band$V1))

r_band = read.table('DATA/NGC6522_R7000_2016-04-02.dat')
temp <- fit_cont(x = r_band$V1, y = r_band$V2, nsig_low = 1.5)
r_band_norm = r_band$V2/predict(temp, newdata = data.frame(x1_temp = r_band$V1))

b_band = read.table('DATA/NGC6522_B7000_2016-04-02.dat')
temp <- fit_cont(x = b_band$V1, y = b_band$V2, nsig_low = 1.5)
b_band_norm = b_band$V2/predict(temp, newdata = data.frame(x1_temp = b_band$V1))

i_band = read.table('DATA/NGC6522_I7000_2016-04-02.dat')
temp <- fit_cont(x = i_band$V1, y = i_band$V2, nsig_low = 1.5)
i_band_norm = i_band$V2/predict(temp, newdata = data.frame(x1_temp = i_band$V1))

# ------------------------------------------------------------------------
# PLOT SPECTRA
# ------------------------------------------------------------------------
delta_l_plot = 100 # A
NN = as.integer((lmax - lmin) / delta_l_plot)

linelist = read.csv('linelist.csv')

pdf(output_fig, width = 12, height = 5)
par(mar = c(5, 5, 1, 1), cex.lab = 1.5, cex.axis = 1.5)
par(font = list(family = 'Times'))

for(i in 1:NN){
  xmin = (i - 1) * delta_l_plot + lmin
  xmax = xmin + delta_l_plot
  
  plot(wave, flux_norm, type = 'l', lty = 1, lwd = 2, xlab = expression(paste(lambda, ' (', ring(A), ')')),
       ylab = 'Normalised Flux', ylim =  c(0.5, 1.1), xlim = c(xmin, xmax), log = 'y')
  
  if(compare == T){
    lines(wavecomp, flux_normcomp, col = 'blue', lwd = 2, lty = 5)
  }
  
  vv = 10
  lines(u_band$V1 * (1 + vv/3e5), u_band_norm, type = 'l', col ='red', lwd = 1, lty = 1)
  lines(r_band$V1 * (1 + vv/3e5), r_band_norm, type = 'l', col ='red', lwd = 1, lty = 1)
  lines(b_band$V1 * (1 + vv/3e5), b_band_norm, type = 'l', col ='red', lwd = 1, lty = 1)
  lines(i_band$V1 * (1 + vv/3e5), i_band_norm, type = 'l', col ='red', lwd = 1, lty = 1)
  
  if(compare == T){
    legend("bottomright", legend = c(legend_comp1, legend_comp2, 'Observed'), col = c('black', 'blue', 'red'), 
           lty = c(1, 1), lwd = c(2.5, 2.3), bty = 'n')
  }else{
    legend("bottomright", legend = c('Model', 'Observed'), col = c('black', 'red'), 
           lty = c(1, 1), lwd = c(2.5, 2.3), bty = 'n')
  }
  
  lambdas = linelist$lambda[linelist$lambda >= xmin & linelist$lambda <= xmax]
  elements = linelist$elem[linelist$lambda >= xmin & linelist$lambda <= xmax]
  kk = 1
  for(ll in lambdas){
    abline(v = ll, col = 'magenta', lty = 5)
    legend(ll-3, 1.1, elements[kk], cex = 0.8, bty = 'n')
    kk = kk + 1
  }
}

dev.off()


   
   
   
