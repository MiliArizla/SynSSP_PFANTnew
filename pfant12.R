# ************************************
# CHANGE LOG
# ***********************************
# v7: Nulbad input (--fn_flux) is the spectra (not the normalised one)
# 25/07/18 : New grid atmospheric models ---> change in innewmarcs call
#            addition of parameter fn_modgrid
# Tatiana 

print('------ FUNCTION PFANT12 ------')
print('------------------------------')
print('PURPOSE:')
print('   Calculate synthetic stellar spectra using the PFANT code')
print('CALLING SEQUENCE:')
print('   pfant12(parfile, feh, afe, lmin, lmax, age, vt, fwhm, dl, CFe, CFe_rgb, NFe, NFe_rgb,')
print('           OFe, MgFe, SiFe, CaFe, TiFe, NaFe, AlFe, BaFe, EuFe, n_ms, n_rg, logg_cn, parfile)')
print('INPUTS:')
print('   feh  = iron abundance [Fe/H]')
print('   afe  = [alpha/Fe]')
print('   lmin = lower lambda')
print('   lmax = upper lambda')
print('   age  = age of the isochrone (Gyr)')
print('   vt   = microturbulence velocity (default = 2.0 km/s)')
print('   fwhm = spectral resolution (default = 0.2 A)')
print('   dl   = sampling delta lambda (default = 0.1 A/pixel)')
print('   CFe  = [C/Fe] (set to 0.0 if omitted, i.e., default [C/Fe] = solar)')
print('CFe_rgb = [C/Fe] of RGBs with logg <= logg_cn (set to CFe if omitted, i.e., default [C/Fe] = CFe)')
print('   NFe  = [N/Fe] (set to 0.0 if omitted, i.e., default [N/Fe] = solar)')
print('NFe_rgb = [N/Fe] of RGBs with logg <= logg_cn (set to NFe if omitted, i.e., default [N/Fe] = NFe)')
print('   OFe  = [O/Fe] (set to [alpha/Fe] if omitted, i.e., default [O/Fe] = afe)')
print('   MgFe = [Mg/Fe] (set to [alpha/Fe] if omitted, i.e., default [Mg/Fe] = afe)')
print('   SiFe = [Si/Fe] (set to [alpha/Fe] if omitted, i.e., default [Si/Fe] = afe)')
print('   CaFe = [Ca/Fe] (set to [alpha/Fe] if omitted, i.e., default [Ca/Fe] = afe)')
print('   TiFe = [Ti/Fe] (set to a[alpha/Fe]fe if omitted, i.e., default [Ti/Fe] = afe)')
print('   NaFe = [Na/Fe] (set to 0.0 if omitted, i.e., default [Na/Fe] = solar)')
print('   AlFe = [Na/Fe] (set to 0.0 if omitted, i.e., default [Al/Fe] = solar)')
print('   BaFe = [Na/Fe] (set to 0.0 if omitted, i.e., default [Ba/Fe] = solar)')
print('   EuFe = [Na/Fe] (set to 0.0 if omitted, i.e., default [Eu/Fe] = solar)')
print('   n_ms = number of main sequence stars')
print('   n_rg = number of red giant stars')
print('logg_cn = stars with logg <= logg_cn --> RGBs with different [C/Fe] and [N/Fe] ratios (CFe_rgb and NFe_rgb), default logg_cn = 3.0')
print('   parfile = file with list of stellar parameters (used only if n_ms/n_rg are not specified)')
print('OUTPUT:')
print('   Synthetic stellar spectra in folder ./Stellar_Spectra/' )
print('   logfile: pfant12.log' )
print('REQUIRED SCRIPTS:')
print('   pfant (fortran code)')
print('   nulbad (fortran code)')
print('   hydro2 (fortran code)')
print('   innewmarcs (fortran code)')
print('   set.stpars.filename (R function, defined in stpars.R)')
print('EXAMPLE:')
print('   pfant12(0.2, 0.2, 6000, 6500, 8, n_ms = 9, n_rg = 6)')

# Defining PATH to PYFANT
Sys.setenv(PATH=paste(Sys.getenv("PATH"), "/home/trevisan/PFANT/fortran/bin/", sep=":"))

####################################################################
# FUNCTION PFANT12
####################################################################
pfant12 <- function(feh, afe, lmin, lmax, age, vt, fwhm, dl, CFe, CFe_rgb, NFe, NFe_rgb,
                    OFe, MgFe, SiFe, CaFe, TiFe, NaFe, AlFe, BaFe, EuFe, n_ms, n_rg, logg_cn, parfile){
  
    logfile = 'pfant12.log'

    #---------------------------------
    # CHECKING INPUTS
    #---------------------------------
    if(missing(feh)){print('Need to specify [Fe/H] (feh=)')}
    if(missing(afe)){print('Need to specify [alpha/Fe] (afe=)')}
    if(missing(lmin) | missing(lmax)){print('Need to specify wavelength range (lmin=, lmax=)')}
    if(missing(age)){print('Need to specify population age in Gyr (age=)')}
    
    if(missing(n_ms) & missing(n_rg) & missing(parfile)){
      print('FILE WITH STELLAR PARAMETERS --> ???')
      print(' Need to specify n_ms & n_rg OR parfile')
    }else{
      if(missing(n_ms) == F & missing(n_rg) == F){
        pars.file = set.stpars.filename(n_ms, n_rg, feh, afe, age)
      }else{
        pars.file = parfile
      }
    }
   
    #---------------------------------
    # SETTING DEFAULT VALUES
    #---------------------------------
    if(missing(CFe)){CFe = 0.0}
    if(missing(CFe_rgb)){CFe_rgb = CFe}
    if(missing(NFe)){NFe = 0.0}
    if(missing(NFe_rgb)){NFe_rgb = NFe}
    if(missing(OFe)){OFe = afe}
    if(missing(MgFe)){MgFe = afe}
    if(missing(SiFe)){SiFe = afe}
    if(missing(CaFe)){CaFe = afe}
    if(missing(TiFe)){TiFe = afe}
    if(missing(NaFe)){NaFe = 0.0}
    if(missing(AlFe)){AlFe = 0.0}
    if(missing(BaFe)){BaFe = 0.0}
    if(missing(EuFe)){EuFe = 0.0}
    if(missing(fwhm)){fwhm = 0.2}
    if(missing(dl)){dl = 0.1}
    if(missing(vt)){vt = 2.0}
    if(missing(logg_cn)){logg_cn = 3}

    if(CFe != CFe_rgb | NFe != NFe_rgb){
      print(sprintf('RGBs with logg <= %.1f --> [C/Fe] = %.2f and [N/Fe] = %.2f', logg_cn, CFe_rgb, NFe_rgb))
    }
    
    #---------------------------------
    # WRITE INFO IN LOGFILE
    #---------------------------------
    write(paste('----------------------- INPUT PARAMETERS -----------------------'), file = logfile, append = T)
    write(paste('----------------------------------------------------------------'), file = logfile, append = T)
    
    write(paste('System time: ', Sys.time()), file = logfile, append = T)
    
    temp <- sprintf('%-26s%8.2f%4s', 'Isochrone age: ', age, ' Gyr')
    write(temp, file = logfile, append = T)
    temp <- sprintf('%-26s%8.0f%3s%6.0f%2s', 'Wavelength limits: ', lmin, ' - ', lmax, ' A')
    write(temp, file = logfile, append = T)
    temp <- sprintf('%-26s%8.2f%5s', 'Microturbulence velocity: ', vt, ' km/s')
    write(temp, file = logfile, append = T)
    temp <- sprintf('%-26s%8.2f%2s', 'FWHM: ', fwhm, ' A')
    write(temp, file = logfile, append = T)
    
    write(paste('Abundances:'), file = logfile, append = T)
    temp <- sprintf('%8s%8s%8s%8s%8s%8s%8s%8s%8s%8s%8s%8s%8s', '[Fe/H]', '[a/Fe]', 
                  '[C/Fe]', '[N/Fe]', '[O/Fe]', '[Mg/Fe]', '[Si/Fe]', '[Ca/Fe]', '[Ti/Fe]', '[Na/Fe]', '[Al/Fe]', '[Ba/Fe]', '[Eu/Fe]')
    write(temp, file = logfile, append = T)
  
    temp <- sprintf('%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f', 
                  feh, afe, CFe, NFe, OFe, MgFe, SiFe, CaFe, TiFe, NaFe, AlFe, BaFe, EuFe)
    write(temp, file = logfile, append = T)
    
    if(CFe != CFe_rgb | NFe != NFe_rgb){
      write(' ', file = logfile, append = T)
      temp <- sprintf('RGBs with logg <= %.1f --> [C/Fe] = %.2f and [N/Fe] = %.2f', logg_cn, CFe_rgb, NFe_rgb)
      write(temp, file = logfile, append = T)
    }
    
    #---------------------------------
    # READ INPUT FILE
    #---------------------------------
    t = read.table(pars.file)
    Teffs = t$V1
    loggs = t$V2
    phase = t$V5
    nstars = nrow(t)
    
    write(paste('Reading stellar parameters from: ', pars.file), file = logfile, append = T)
    write(paste('----------------------------------------------------------------\n'), file = logfile, append = T)
    write(paste('----------------------------------------------------------------\n'), file = logfile, append = T)
    
    #---------------------------------
    # CALCULATE SYNTHETIC SPECTRA
    #---------------------------------
    for(i in c(1:nstars)){
      #---------------------------------
      # SETTING FILE NAME
      #---------------------------------
      if(phase[i] != 'rgb_cn'){
        file_flux <- set.stspec.filename(feh, afe, lmin, lmax, Teffs[i], 
                                         loggs[i], CFe, NFe, OFe, MgFe, SiFe, CaFe, TiFe, NaFe, AlFe, BaFe, EuFe)
      }else{
        file_flux <- set.stspec.filename(feh, afe, lmin, lmax, Teffs[i], 
                                         loggs[i], CFe_rgb, NFe_rgb, OFe, MgFe, SiFe, CaFe, TiFe, NaFe, AlFe, BaFe, EuFe)
      }
        
      file_fig  = paste(file_flux, '.jpg', sep = '') 
      
      file_flux_cont = paste(file_flux, '_cont', sep = '')
      file_flux_norm = paste(file_flux, '_norm', sep = '')
      file_flux_convol = paste(file_flux, '_convol', sep = '')
      
      #---------------------------------
      # CHECK IF SPECTRUM EXISTS
      #---------------------------------
      if(!file.exists('Stellar_Spectra')){system('mkdir Stellar_Spectra')}
      flag.missing = 0
      flag.missing = file.access(file_flux, mode = 0)
      if(flag.missing == 0){
        write(paste('Synthetic stellar spectra: (Teff, logg) = (', Teffs[i], ', ', loggs[i], ')', 
                    ' already exists --> skipping', sep = ''), file = logfile, append = T)
        write(paste('     file: ', file_flux, sep = ''), file = logfile, append = T)
        write(paste('----------------------------------------------------------------\n'), file = logfile, append = T)
      }else{
        write(paste('Calculating synthetic stellar spectra: (Teff, logg) = (', Teffs[i], ', ', loggs[i], ')', sep = ''),
              file = logfile, append = T)
        write(paste('Starting at: ', Sys.time()), file = logfile, append = T)
        ptm <- proc.time()
        
        if(phase[i] != 'rgb_cn'){ 
          write.abonds(CFe, NFe, OFe, MgFe, SiFe, CaFe, TiFe, NaFe, AlFe, BaFe, EuFe)
        }else{
          write.abonds(CFe_rgb, NFe_rgb, OFe, MgFe, SiFe, CaFe, TiFe, NaFe, AlFe, BaFe, EuFe)
        }
        
        vt_s = vt
        Teff_s = Teffs[i]
        logg_s = loggs[i]
        met_grade_s = feh
        
        main_name = 'main.dat'
        #---------------------------------
        # WRITE MAIN.DAT
        #---------------------------------
        l1 = 'Sun'
        l2 = paste('T ', dl, ' 5.0 1.   ',fwhm)
        l3 = vt_s
        l4 = paste(Teff_s, logg_s, met_grade_s, '0.1 1')
        l5 = 'T  1.'
        l6 = met_grade_s
        l7 = '  '
        l8 = 'flux'
        l9 = paste(lmin, lmax, ' 50')

        data = c(l1, l2, l3, l4, l5, l6, l7, l8, l9)
        write(data, file = main_name, sep = '\n', append = F)
        
        #---------------------------------
        # INTERPOLATION MARCS MODELS
        #---------------------------------
        
        innewmarcs_call = sprintf("innewmarcs --fn_modgrid grid.mod --allow T --fn_main %s --fn_modeles modeles.mod --fn_progress progress.txt --opa F", main_name)
        system(innewmarcs_call)
        
        #---------------------------------
        # CALCULATE HYDROGEN LINES
        #---------------------------------
        hydro2_call = sprintf("hydro2 --fn_hmap hmap.dat --fn_main %s --fn_modeles modeles.mod --fn_progress progress.txt --opa F", main_name)
        system(paste(hydro2_call))
        
        #---------------------------------
        # RUN THE MAIN CODE (PFANT)
        #---------------------------------
        pfant_call =  sprintf("pfant --flprefix flux --fn_abonds abonds.dat --fn_dissoc dissoc.dat --fn_hmap hmap.dat --fn_main %s --fn_modeles modeles.mod --fn_progress progress.txt  --opa F", main_name)
        system(pfant_call)
        
        #---------------------------------
        # CONVOLUTION
        #---------------------------------
        nulbad_call = sprintf("nulbad --fn_flux flux.spec --flprefix flux --fn_progress progress.txt")
        system(paste(nulbad_call))
        
        #---------------------------------
        # WRITE STELLAR SPECTRA
        #---------------------------------
        system(paste('mv flux.spec ', file_flux, sep = '')) 
        system(paste('mv flux.cont ', file_flux_cont, sep = '')) 
        system(paste('mv flux.norm ', file_flux_norm, sep = '')) 
        system(sprintf('mv flux.spec.nulbad.%1.3f %s', fwhm, file_flux_convol)) 

        #---------------------------------
        # PLOT STELLAR SPECTRA
        #---------------------------------
        t = read.table(file_flux_convol)
        jpeg(file_fig, width = 800, height = 500)
        plot(t$V1, t$V2, type = 'l', xlab = expression(paste(lambda, ' (', ring(A), ')')), 
             ylab = 'Flux')
        dev.off()
        
        #---------------------------------
        # WRITE INFO IN LOGFILE
        #---------------------------------
        time <- proc.time() - ptm 
        write(paste('Finishing at: ', Sys.time()), file = logfile, append = T)
        write(sprintf('%14s%10.0f%4s%-10.3f%5s', 'Elapsed time: ', time[3], ' s (', time[3]/60, ' min)'), 
              file = logfile, append = T)
        write(paste('----------------------------------------------------------------\n'), file = logfile, append = T)
      }
    }  
}

##################################
# FUNCTION WRITE.ABONDS
##################################
write.abonds <- function(CFe, NFe, OFe, MgFe, SiFe, CaFe, TiFe, NaFe, AlFe, BaFe, EuFe){
  # ABUNDANCES: [X/H] -> STELLAR IRON ABUNDANCE [Fe/H] ALREADY ADDED
  #    (VALUES IN DISSOC AND ABONDS FILES ARE (X/H) - [Fe/H])
  # INITIAL ABUNDANCES (SOLAR)
  abonds.ini.file = 'abonds_Sun.dat'

  # READ INITIAL ABUNDANCES
  #---------------------------------
  # ABONDS.DAT
  t = read.table(abonds.ini.file, fill = T, as.is = 1)
  NABOND = length(t$V1) - 2
  
  elems = t$V1[1:NABOND]
  elems <- sprintf("%2s", elems)
  
  abonds.ini = t$V2[1:NABOND]
  abonds.fin = abonds.ini
  for(k in 1:NABOND){
    if(elems[k] == ' C'){abonds.fin[k] = CFe + abonds.ini[k]}
    if(elems[k] == ' N'){abonds.fin[k] = NFe + abonds.ini[k]}
    if(elems[k] == ' O'){abonds.fin[k] = OFe + abonds.ini[k]}
    if(elems[k] == 'MG'){abonds.fin[k] = MgFe + abonds.ini[k]}
    if(elems[k] == 'SI'){abonds.fin[k] = SiFe + abonds.ini[k]}
    if(elems[k] == 'CA'){abonds.fin[k] = CaFe + abonds.ini[k]}
    if(elems[k] == 'TI'){abonds.fin[k] = TiFe + abonds.ini[k]}
    if(elems[k] == 'NA'){abonds.fin[k] = NaFe + abonds.ini[k]}
    if(elems[k] == 'AL'){abonds.fin[k] = AlFe + abonds.ini[k]}
    if(elems[k] == 'BA'){abonds.fin[k] = BaFe + abonds.ini[k]}
    if(elems[k] == 'EU'){abonds.fin[k] = EuFe + abonds.ini[k]}
  } # END for(k in 1:NABOND)

  #---------------------------------
  # WRITE ABONDS.DAT
  #---------------------------------
  data <- sprintf("%3s%6.2f", elems, abonds.fin)
  write(data, file = 'abonds.dat', sep = '\n', append = F)
  write(c(1, 1), file = 'abonds.dat', sep = '\n', append = T)
}

##################################
# FUNCTION SET.STSPEC.FILENAME
##################################
set.stspec.filename <- function(feh, afe, lmin, lmax, Teff, logg, CFe, NFe, OFe, MgFe, SiFe, CaFe, TiFe, NaFe, AlFe, BaFe, EuFe){
  temp = c(feh, afe, CFe, NFe, OFe, MgFe, SiFe, CaFe, TiFe, NaFe, AlFe, BaFe, EuFe)
  temp_s = vector()
  for(j in 1:length(temp)){
    if(temp[j] < 0){
      temp_s[j] = sprintf('%1s%4.2f', '-', abs(temp[j]))
    }else{
      temp_s[j] = sprintf('%1s%4.2f', '+', temp[j])
    }
  }
  
  Teff_s = sprintf('%4.0f', Teff)
  if(logg < 0){
    logg_s = sprintf('%1s%4.2f', '-', abs(logg))
  }else{
    logg_s = sprintf('%1s%4.2f', '+', logg)
  }
  
  lmin_s = sprintf('%4.0f', lmin)
  if(lmax > 1e4){lmax_s = sprintf('%5.0f', lmax)}else{lmax_s = sprintf('%4.0f', lmax)}
  
  file = paste('./Stellar_Spectra/flux_Fe', temp_s[1],  '_a', temp_s[2], '_C', temp_s[3], '_N', temp_s[4],
                      '_O', temp_s[5],'_Mg', temp_s[6], '_Si', temp_s[7],'_Ca', temp_s[8],'_Ti', temp_s[9], 
                     '_Na', temp_s[10], '_Al', temp_s[11], '_Ba', temp_s[12], '_Eu', temp_s[13], 
                     '_T', Teff_s, '_g', logg_s, '_', lmin_s, '-', lmax_s, sep = '')
  return(file)
}

