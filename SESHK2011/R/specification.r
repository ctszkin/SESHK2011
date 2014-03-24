#' Description genSpecStandardMath
#' @name genSpecStandardMath
#' @aliases genSpecStandardMath
#' @title genSpecStandardMath
#' @param network network = "all" gives friends and studymates networks. network = "friends" gives friends network only. network = studymates gives studymates network only.
#' @param network_formation_fixed_effect Default=FALSE
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export

genSpecStandardMath <- function(network=c("all","friends","studymates"), network_formation_fixed_effect = FALSE){

  network = match.arg(network)
  
  data_version = "SESHK2011 - network - 0.8.0"

  school_names <- c("kslo","lskc","thf1","thf2","thf3")

  ## findDropCaseID
  ## return a vector of drop_case_ID
  ## data_wide is the data matrix
  ## network_matrix_list is a list of network matrix computed from process_network
  findDropCaseID = function(data_wide, network_matrix_list){
    drop_case_ID = c("idlskcE030000" , "idthf3A080063" , "idthf3E080026" , "idthf2B080100", "idthf2B091598")


    ### using data_wide to find drop_case_id
    # drop_case_ID = c(
    #   drop_case_ID, 
    #   with( data_wide, case_id[ yob == 1997 ])
    #  )
    ### using network_matrix_list to drop case_id. In this example, Those with 0 connection will be dropped. data_wide and the network_matrix has same index position.
    ### 
    # index = unlist( lapply(network_matrix_list, function(x) which(rowSums(x)==0) ) )
    # drop_case_ID = c(drop_case_ID, data_wide$case_id[ index ] )


    unique(drop_case_ID)
  }

  # hobby = c("art","ballsport","chinesemusic","class","detail","everything","interest","music","sport","team","westernmusic" )

  network_formation_fixed_effect = network_formation_fixed_effect

  ## in the process network function, you can design the value of the network matrix. 
  ## For example, you can set the network connection to be 0 if data_wide$tutorial of both agent are not zero
  # process_network_function_example = function(x, y, data_wide){

  #   out2 = outer(data_wide$tutorial, data_wide$tutorial ) > 0

  #   n = ncol(x)
  #   xx <- (x > 0) + 0
  #   yy <- (y > 0) + 0
  #   out <- ((xx) & t(xx))

  #   out * out2
  # }

  # homework_help_classmates
  # process_network_function_friends <- function(x,y, data_wide){
  #   xx <- (x > 0) + 0
  #   yy <- (y > 0) + 0
  #   #out <- (t(xx) | (yy)) & (t(yy) | (xx))
  #   #out <- ((xx) & t(xx))
  #   out <- ((xx) & t(xx))
  #   diag(out) = 0
  #   out
  # }


  # process_network_function_studymates <- function(x,y, data_wide){
  #   #hhc = (data_wide$homework_help_classmates > 2) + 0 
  #   #out2 = outer(hhc, hhc ) > 0
    
  #   xx <- (x > 0) + 0
  #   yy <- (y > 0) + 0
  #   #out <- (t(xx) | (yy)) & (t(yy) | (xx))
  #   #out <- ((xx) | (yy)) | (t(yy) | t(xx))
  #   #out <- ((xx) & t(yy)) | ((yy) & t(xx))
  #   #out <- (((xx) & t(yy)) | ((yy) & t(xx)) | ((xx) & (yy)) | (t(xx) & t(yy)) | (xx) | t(xx))
  #   #out <- ((yy) & t(yy))
    
  #   out <- (((xx) & t(xx)) | ((yy) & t(yy)))
    
  #   diag(out) = 0
  #   #out*out2
  #   out
  # }

  ## New version allow more than one raw network matrix. The raw network matrix are stored in a list. The previous version of process_network_function is still working but it's better to follow the new format.

  process_network_function_friends <- function(network_matrix_list, data_wide){
  	x = network_matrix_list[[1]]
  	y = network_matrix_list[[2]]
    xx <- (x > 0) + 0
    yy <- (y > 0) + 0
    #out <- (t(xx) | (yy)) & (t(yy) | (xx))
    #out <- ((xx) & t(xx))
    out <- ((xx) & t(xx))
    diag(out) = 0
    out
  }
  process_network_function_studymates <- function(network_matrix_list, data_wide){
  	x = network_matrix_list[[1]]
  	y = network_matrix_list[[2]]
    #hhc = (data_wide$homework_help_classmates > 2) + 0 
    #out2 = outer(hhc, hhc ) > 0
    
    xx <- (x > 0) + 0
    yy <- (y > 0) + 0
    #out <- (t(xx) | (yy)) & (t(yy) | (xx))
    #out <- ((xx) | (yy)) | (t(yy) | t(xx))
    #out <- ((xx) & t(yy)) | ((yy) & t(xx))
    #out <- (((xx) & t(yy)) | ((yy) & t(xx)) | ((xx) & (yy)) | (t(xx) & t(yy)) | (xx) | t(xx))
    #out <- ((yy) & t(yy))
    
    out <- (((xx) & t(xx)) | ((yy) & t(yy)))
    
    diag(out) = 0
    #out*out2
    out
  }






  genNetworkStatistics <- function(network_matrix_list){
    network_name = names(network_matrix_list)

    out = sapply(network_matrix_list, rowSums)

    colnames(out) = paste0("degree_", network_name)

    out
  }



  formula <- Formula( exam1_math_std ~ matrix_test_std + bfi_conscientiousness_std + bfi_openness_std + bfi_agreeableness_std + bfi_neuroticism_std + bfi_extraversion_std  + male  + commute_car + commute_taxi + ksloB + ksloC + ksloD + ksloE +  lskcB + lskcC + lskcD + lskcE + thf1B + thf1C + thf1D + thf1E + thf2B + thf2C + thf2D + thf2E + thf3B + thf3C + thf3D + thf3E + height_std + I(elder_brother+elder_sister) + I(younger_brother+younger_sister) + I((play_any_music>0)+0) + I((play_any_sport>0)+0) + I((play_team>0)+0) | matrix_test_std + bfi_conscientiousness_std +  bfi_openness_std + bfi_agreeableness_std + bfi_neuroticism_std + bfi_extraversion_std  )

  formula = 
  switch(network,
    all = update(formula, .~.+ degree_friends + degree_studymates |.),
    friends = update(formula, .~.+ degree_friends  |.),
    studymates = update(formula, .~.+ degree_studymates |.)
  )
  
  network_formation_formula <- (
    ~ matrix_test_std 
    + I(abs(matrix_test_std - friends_matrix_test_std)) 
    + bfi_conscientiousness_std 
    + I(abs(bfi_conscientiousness_std - friends_bfi_conscientiousness_std)) 
    + bfi_openness_std 
    + I(abs(bfi_openness_std - friends_bfi_openness_std)) 
    + bfi_agreeableness_std 
    + I(abs(bfi_agreeableness_std - friends_bfi_agreeableness_std)) 
    + bfi_neuroticism_std + I(abs(bfi_neuroticism_std - friends_bfi_neuroticism_std)) 
    + bfi_extraversion_std + I(abs(bfi_extraversion_std - friends_bfi_extraversion_std)) 
    + male 
    + I(abs(male-friends_male)) 
    + commute_taxi + I(abs(commute_taxi - friends_commute_taxi))
    + commute_car + I(abs(commute_car - friends_commute_car))
    + height_std + I(abs(height_std - friends_height_std)) 
    + I(elder_brother+elder_sister) + I(younger_brother+younger_sister) 
    + I(abs(((play_any_music>0)+0)-((friends_play_any_music>0)+0))) 
    + I(abs(((play_any_sport>0)+0)-((friends_play_any_sport>0)+0))) 
    + I(abs(((play_team>0)+0)-((friends_play_team>0)+0)))
    + I( (class_n == friends_class_n)  +0 ) 
    + I( (violin == friends_violin) + (piano == friends_piano) + (guitar == friends_guitar) + (harp == friends_harp) + (horn == friends_horn) + (flute == friends_flute) + (clarinet == friends_clarinet) + (cello == friends_cello) + (melodica == friends_melodica) + (bell == friends_bell) + (erhu == friends_erhu) + (guzheng == friends_guzheng) + (pipa == friends_pipa) + (yangqin == friends_yangqin) + (chinese_flute == friends_chinese_flute) + (yuan == friends_yuan) + (chinese_lo == friends_chinese_lo) + (recorder == friends_recorder) + (xylophone == friends_xylophone) + (keyboard == friends_keyboard) + (electric_guitar == friends_electric_guitar) + (harmonica == friends_harmonica) + (africa_drum == friends_africa_drum) + (percussion == friends_percussion) + (saxophone == friends_saxophone) + (drum == friends_drum) + (ocarina == friends_ocarina) )
    + I( (boyscout == friends_boyscout) + (girlguide == friends_girlguide) + (boybrigade == friends_boybrigade) + (girlbrigade == friends_girlbrigade) + (stjohn == friends_stjohn) + (jpc == friends_jpc) + (sst == friends_sst) + (cyc == friends_cyc) + (rsp == friends_rsp) + (red_cross == friends_red_cross) + (flag == friends_flag) + (prefect == friends_prefect) + (Teen == friends_Teen) + (leadership == friends_leadership) + (civilaid == friends_civilaid) + (cheerlead == friends_cheerlead) + (volunteer == friends_volunteer) + (marchingband == friends_marchingband) + (britishcouncil == friends_britishcouncil) )
    + I( (basketball == friends_basketball) + (soccer == friends_soccer) + (pingpong == friends_pingpong) + (badminton == friends_badminton) + (volleyball == friends_volleyball) + (golf == friends_golf) + (bowling == friends_bowling) + (tennis == friends_tennis) + (squash == friends_squash) + (rugby == friends_rugby) + (dodgeball == friends_dodgeball) + (handball == friends_handball) + (ropeskip == friends_ropeskip) + (mountaineer == friends_mountaineer) + (athletics == friends_athletics) + (swim == friends_swim) + (skiing == friends_skiing) + (rowing == friends_rowing) + (hurdle == friends_hurdle) + (archery == friends_archery) + (cycling == friends_cycling) + (chinese_dance == friends_chinese_dance) + (latin_dance == friends_latin_dance) + (dance == friends_dance) + (ballet == friends_ballet) + (karate == friends_karate) + (Taekwondo == friends_Taekwondo) + (Yudo == friends_Yudo) + (kungfu == friends_kungfu) + (taiqi == friends_taiqi) + (lion == friends_lion) + (run == friends_run) + (gun == friends_gun) + (slideboard == friends_slideboard) + (yoyo == friends_yoyo) + (shotput == friends_shotput) + (gymnastics == friends_gymnastics) + (fencing == friends_fencing) + (climbing == friends_climbing) + (jianzi == friends_jianzi) )
  ) 

    # | music + sport + team + class) 


	# If you want to have more than two raw network matrices, you can set definition=c("network_1","network_2","network_3")
	# Of course, you can just use one network!

  friends_network_info=list(
    name="friends",
    definition=c("my_friends","my_friends"),
    process_network=process_network_function_friends)

  studymates_network_info=list(
    name="studymates",
    definition=c("help_me_school","help_them_school"),
    process_network=process_network_function_studymates)

  network_info_list=
  	switch(network, 
        all = list(friends=friends_network_info, studymates= studymates_network_info),
        friends = list(friends=friends_network_info),
        studymates = list(studymates=studymates_network_info)
      )

  out = list(
    data_version=data_version,
    school_names=school_names, 
    # hobby=hobby,
    findDropCaseID=findDropCaseID, 
    formula=formula, 
    network_formation_formula = network_formation_formula, 
    network_info_list=network_info_list,
    genNetworkStatistics=genNetworkStatistics,
    network_formation_fixed_effect=network_formation_fixed_effect
  )
  class(out) = "SESHK_Spec"
  out


}

