source('stpars.R')
source('pfant12.R')
source('SSP_model.R')

#---------------------------------
# Set parameters
#---------------------------------
feh = -1.0       # [Fe/H]
afe = 0.3        # [alpha/Fe]
CFe = -0.03          
NFe = 0.67         
SiFe = 0.13
CaFe = 0.13
TiFe = 0.10
CFe_rgb = -0.2      
NFe_rgb = 1.0     
BaFe = 0.02         
EuFe = -0.14         
age = 11          # age (Gyr)
imf = 'Salpeter'  # IMF
lmin = 4500       # lower lambda
lmax = 6800       # upper lambda

OFe_1 = 0.4
MgFe_1 = 0.4
NaFe_1 = 0.0
AlFe_1 = 0.0

OFe_2 = 0.0
MgFe_2 = 0.0
NaFe_2 = 0.4
AlFe_2 = 0.4

file_ssp_1 <- set.ssp.filename(feh, afe, age = age, imf = imf, CFe, NFe = NFe, OFe = OFe_1,
                                  TiFe = TiFe, SiFe = SiFe, MgFe = MgFe_1, CaFe = CaFe, NaFe = NaFe_1, BaFe = BaFe, AlFe = AlFe_1, EuFe = EuFe, 
                                  CFe_rgb = CFe_rgb, NFe_rgb = NFe_rgb)

file_ssp_2 <- set.ssp.filename(feh, afe, age = age, imf = imf, CFe, NFe = NFe, OFe = OFe_2,
                                    TiFe = TiFe, SiFe = SiFe, MgFe = MgFe_2, CaFe = CaFe, NaFe = NaFe_2, BaFe = BaFe, AlFe = AlFe_2, EuFe = EuFe, 
                                    CFe_rgb = CFe_rgb, NFe_rgb = NFe_rgb)

spec = read.table(file_ssp_1)
wave = spec$V1
flux_1 = spec$V2

spec = read.table(file_ssp_2)
wave = spec$V1
flux_2 = spec$V2

flux_total = f1 * flux_1 + f2 * flux_2

temp <- fit_cont(x = wave, y = flux_total)

cont = predict(temp, newdata = data.frame(x1_temp = wave))
flux_total_norm = flux_total / cont

############################################
# USHER spectra
u_band = read.table('NGC6522_U7000_2016-04-02.dat')
temp <- fit_cont(x = u_band$V1, y = u_band$V2, nsig_low = 1.5)
u_band_norm = u_band$V2/predict(temp, newdata = data.frame(x1_temp = u_band$V1))

r_band = read.table('NGC6522_R7000_2016-04-02.dat')
temp <- fit_cont(x = r_band$V1, y = r_band$V2, nsig_low = 1.5)
r_band_norm = r_band$V2/predict(temp, newdata = data.frame(x1_temp = r_band$V1))

b_band = read.table('NGC6522_B7000_2016-04-02.dat')
temp <- fit_cont(x = b_band$V1, y = b_band$V2, nsig_low = 1.5)
b_band_norm = b_band$V2/predict(temp, newdata = data.frame(x1_temp = b_band$V1))

i_band = read.table('NGC6522_I7000_2016-04-02.dat')
temp <- fit_cont(x = i_band$V1, y = i_band$V2)
i_band_norm = i_band$V2/predict(temp, newdata = data.frame(x1_temp = i_band$V1))

delta_l_plot = 100 # A
NN = as.integer((lmax - lmin) / delta_l_plot)

linelist = read.csv('linelist.csv')

# Plotting  the spectra
pdf("NGC6522_SSP.pdf", width = 12, height = 5)
par(mar = c(5, 5, 1, 1), cex.lab = 1.5, cex.axis = 1.5)
par(font = list(family = 'Times'))

for(i in 1:NN){
  xmin = (i - 1) * delta_l_plot + lmin
  xmax = xmin + delta_l_plot
  
  plot(wave, flux_norm_1, type = 'l', lty = 1, lwd = 2, xlab = expression(paste(lambda, ' (', ring(A), ')')),
       ylab = 'Normalised Flux', ylim =  c(0.5, 1.1), xlim = c(xmin, xmax), log = 'y')
  
  lines(wave, flux_norm_2, col = 'blue', lwd = 2, lty = 5)

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


