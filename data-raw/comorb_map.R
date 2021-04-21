
#' comorbidity mappings
#'
#' Charlson comorbidity index components
#' @format a list of vectors and dataframes
#'
#' @source mappings are based on the comorbidity-package and own our own mappings

{
  charl_icd09 <-
    list("Myocardial infarction" = c("410", "412"),
         "Congestive heart failure" = c("39891", "40201", "40211", "40291", "40401", "40403", "40411", "40413", "40491", "40493", sprintf("%04d", "4254":"4259"), "428"),
         "Peripheral vascular disease"=  c("0930", "4373", "440", "441", "4431":"4439", "4471", "5571", "5579", "V434"),
         "Cerebrovascular disease" = c("36234", "430":"438"),
         "Dementia" = c("290", "2941", "3312"),
         "Chronic pulmonary disease" = c("4168", "4169", "490:505", "5064", "5081", "5088"),
         "Rheumatic disease" = c("4465", "7100:7104", "7140":"7142", "7148", "725"),
         "Peptic ulcer disease" = sprintf("%03d", "531":"534"),
         "Mild liver disease" = c("07022", "07023", "07032", "07033", "07044", "07054", "0706", "0709", "570", "571", "5733", "5734", "5738", "5739", "V427"),
         "Diabetes without chronic complication" = c("2500":"2503", "2508", "2509"),
         "Diabetes with chronic complication" = sprintf("%04d", "2504":"2507"),
         "Hemiplegia or paraplegia" = c("3341", "342", "343", "3440":"3446", "3449"),
         "Renal disease" = c("40301", "40311", "40391", "40402", "40403", "40412", "40413", "40492", "40493", "582", "5830":"5837", "585", "586", "5880", "V420", "V451", "V56"),
         "Any malignancy, including lymphoma and leukaemia, except malignant neoplasm of skin" = c(sprintf("%03d", 140:172), sprintf("%03d", 174:195), sprintf("%04d", 1950:1958), sprintf("%03d", 200:208), "2386"),
         "Moderate or severe liver disease" = c(sprintf("%04d",4560:4562), sprintf("%04d", 5722:5728)),
         "Metastatic solid tumour" = sprintf("%03d", 196:199), # 196:199
         "AIDS/HIV" = sprintf("%03d", "042":"044"))

  ## ICD-10 codes
  charl_icd10 <-
    list("Myocardial infarction"        = c("I21", "I22", "I252"),
         "Congestive heart failure"     = c("I099", "I110", "I130", "I132", "I255", "I420", paste0("I", 425:429), "I43", "I50", "P290"),
         "Peripheral vascular disease"  = c("I70", "I71", "I731", "I738", "I739", "I771", "I790", "I792", "K551", "K558", "K559", "Z958", "Z959"),
         "Cerebrovascular disease"      = c("G45", "G46", "H340", paste0("I", 60:69)),
         "Dementia"                     = c(paste0("F", sprintf("%02d", 00:03)), "F051", "G30", "G311"),
         "Chronic pulmonary disease"    = c("I278", "I279", paste0("J", 40:47), paste0("J", 60:67), "J684", "J701", "J703"),
         "Rheumatic disease"            = c("M05", "M06", "M315", paste0("M", 32:34), "M351", "M353", "M360"),
         "Peptic ulcer disease"         = c(paste0("K", 25:28)),
         "Mild liver disease"           = c("B18", paste0("K", 700:703), "K709", paste0("K", 713:715), "K717", "K73", "K74", "K760", paste0("K", 762:764), "K768", "K769", "Z944"),
         "Diabetes without chronic complication" = c("E100", "E101", "E106", "E108", "E109", "E110", "E111", "E116", "E118", "E119", "E120", "E121", "E126", "E128", "E129", "E130", "E131", "E136", "E138", "E139", "E140", "E141", "E146", "E148", "E149"),
         "Diabetes with chronic complication" = c(paste0("E", 102:105), "E107", paste0("E", 112:115), "E117", paste0("E", 122:125), "E127", paste0("E", 132:135), "E137", paste0("E", 142:145), "E147"),
         "Hemiplegia or paraplegia"     = c("G041", "G114", "G801", "G802", "G81", "G82", paste0("G", 830:834), "G839"),
         "Renal disease"                = c("I120", "I131", paste0("N", sprintf("%03d", c(032:037, 052:057))), "N18", "N19", "N250", paste0("Z", 490:492), "Z940", "Z992"),
         "Any malignancy, including lymphoma and leukaemia, except malignant neoplasm of skin" = c(paste0("C", sprintf("%02d", 00:26)), paste0("C", c(30:34, 37:41)), "C43", paste0("C", 45:58), paste0("C", 60:76), paste0("C", 81:85), "C88", paste0("C", 90:97)),
         "Moderate or severe liver disease" = c("I850", "I859", "I864", "I982", "K704", "K711", "K721", "K729", "K765", "K766", "K767"),
         "Metastatic solid tumour"      = c(paste0("C", 77:80)),
         "AIDS/HIV"                     = c(paste0("B", 20:22), "B24"))

  charl_names <- c(names(charl_icd10))
  charl_names <-
    data.frame(labels=charl_names,
               names =c("minfar", "heartf", "pervas", "cerebr", "dement", "copdis", "rheuma", "peptic",
                        "liverm", "diabeu", "diabec", "parapl", "renald", "cancer", "liverd", "metaca",
                        "aidshv"
               )
    )
}

## Elixhauser diagnosis classifications
{
  ## ICD-9 codes
  elixh_icd09 <-
    list("Congestive heart failure" = c("39891", "40201", "40211", "40291", "40401", "40403", "40411", "40413", "40491", "40493", "4254":"4259", "428"),
         "Cardiac arrhythmias" = c("4260", "42613", "4267", "4269", "42610", "42612", "4270":"4274", "4276":"4279", "7850", "99601", "99604", "V450", "V533"),
         "Valvular disease" = c("0932", "394":"397", "424", "7463":"7466", "V422", "V433"),
         "Pulmonary circulation disorders" = c("4150", "4151", "416", "4170", "4178", "4179"),
         "Peripheral vascular disorders" = c("0930", "4373", "440", "441", "4431":"4439", "4471", "5571", "5579", "V434"),
         "Hypertension, uncomplicated" = c("401"),
         "Hypertension, complicated" = c("402":"405"),
         "Paralysis" = c("3341", "342", "343", "3440":"3446", "3449"),
         "Other neurological disorders" = c("3319", "3320", "3321", "3334", "3335", "33392", "334":"335", "3362", "340", "341", "345", "3481", "3483", "7803", "7843"),
         "Chronic pulmonary disease" = c("4168", "4169", "490":"505", "5064", "5081", "5088"),
         "Diabetes, uncomplicated" = c("2500":"2503"),
         "Diabetes, complicated" = c("2504":"2509"),
         "Hypothyroidism" = c("2409", "243", "244", "2461", "2468"),
         "Renal failure" = c("40301", "40311", "40391", "40402", "40403", "40412", "40413", "40492", "40493", "585", "586", "5880", "V420", "V451", "V56"),
         "Liver disease" = c("07022", "07023", "07032", "07033", "07044", "07054", "0706", "0709", "4560":"4562", "570", "571", "5722":"5728", "5733", "5734", "5738", "5739", "V427"),
         "Peptic ulcer disease, excluding bleeding" = c("5317", "5319", "5327", "5329", "5337", "5339", "5347", "5349"),
         "AIDS/HIV" = c(sprintf("%03d", "042":"044")),
         "Lymphoma" = c("200":"202", "2030", "2386"),
         "Metastatic cancer" = c("196":"199"),
         "Solid tumour without metastasis" = c("140":"172", "174":"195"),
         "Rheumatoid arthritis/collagen vascular diseases" = c("446", "7010", "7100":"7104", " 7108", "7109", "7112", " 714", "7193", "720", " 725", "7285", "72889", "72930"),
         "Coagulopathy" = c("286", "2871", "2873":"2875"),
         "Obesity" = c("2780"),
         "Weight loss" = c("260":"263", "7832", "7994"),
         "Fluid and electrolyte disorders" = c("2536", "276"),
         "Blood loss anaemia" = c("2800"),
         "Deficiency anaemia" = c("2801":"2809", "281"),
         "Alcohol abuse" = c("2652", "2911":"2913", "2915":"2919", "3030", "3039", "3050", "3575", "4255", " 5353", "5710":"5713", "980", "V113"),
         "Drug abuse" = c("292", "304", "3052":"3059", "V6542"),
         "Psychoses" = c("2938", "295", "29604", "29614", "29644", "29654", "297", "298"),
         "Depression" = c("2962", "2963", "2965", "3004", "309", "311"))

  ## ICD-10 codes
  elixh_icd10 <-
    list("Congestive heart failure" = c("I099", "I110", "I130", "I132", "I255", "I420", paste0("I", 425:429), "I43", "I50", "P290"),
         "Cardiac arrhythmias" = c(paste0("I", 441:443), "I456", "I459", paste0("I", 47:49), "R000", "R001", "R008", "T821", "Z450", "Z950"),
         "Valvular disease" = c("A520", paste0("I", 05:08), "I091", "I098", paste0("I", 34:39), paste0("Q", 230:233), paste0("Z", 952:954)),
         "Pulmonary circulation disorders" = c("I26", "I27", "I280", "I288", "I289"),
         "Peripheral vascular disorders" = c("I70", "I71", "I731", "I738", "I739", "I771", "I790", "I792", "K551", "K558", "K559", "Z958", "Z959"),
         "Hypertension, uncomplicated" = c("I10"),
         "Hypertension, complicated" = c(paste0("I", 1113), "I15"),
         "Paralysis" = c("G041", "G114", "G801", "G802", "G81", "G82", paste0("G", 830:834), "G839"),
         "Other neurological disorders" = c(paste0("G", 10:13), paste0("G", 20:22), "G254", "G255", "G312", "G318", "G319", "G32", paste0("G", 35:37), "G40", "G41", "G931", "G934", "R470", "R56"),
         "Chronic pulmonary disease" = c("I278", "I279", paste0("J", 40:47), paste0("J", 60:67), "J684", "J701", "J703"),
         "Diabetes, uncomplicated" = c("E100", "E101", "E109", "E110", "E111", "E119", "E120", "E121", "E129", "E130", "E131", "E139", "E140", "E141", "E149"),
         "Diabetes, complicated" = paste0("E", c(102:108, 112:118, 122:128, 132:138, 142:148)),
         "Hypothyroidism" = c(paste0("E", sprintf("%02d", 00:03)), "E890"),
         "Renal failure" = c("I120", "I131", "N18", "N19", "N250", paste0("Z", 490:492), "Z940", "Z992"),
         "Liver disease" = c("B18", "I85", "I864", "I982", "K70", "K711", paste0("K", 713:715), "K717", paste0("K", 72:74), "K760", paste0("K", 762:769), "Z944"),
         "Peptic ulcer disease, excluding bleeding" = c("K257", "K259", "K267", "K269", "K277", "K279", "K287", "K289"),
         "AIDS/HIV" = c(paste0("B", 20:22), "B24"),
         "Lymphoma" = c(paste0("C", 81:85), "C88", "C96", "C900", "C902"),
         "Metastatic cancer" = c(paste0("C", 77:80)),
         "Solid tumour without metastasis" = c(paste0("C", sprintf("%02d", 00:26)), paste0("C", c(30:34, 37:41, 43, 45:58, 60:76, 97))),
         "Rheumatoid arthritis/collagen vascular diseases" = c("L940", "L941", "L943", "M05", "M06", "M08", "M120", "M123", "M30", paste0("M", 310:313), paste0("M", 32:35), "M45", "M461", "M468", "M469"),
         "Coagulopathy" = c(paste0("D", 65:68), "D691", paste0("D", 693:696)),
         "Obesity" = c("E66"),
         "Weight loss" = c(paste0("E", 40:46), "R634", "R64"),
         "Fluid and electrolyte disorders" = c("E222", "E86", "E87"),
         "Blood loss anaemia" = c("D500"),
         "Deficiency anaemia" = c("D508", "D509", paste0("D", 51:53)),
         "Alcohol abuse" = c("F10", "E52", "G621", "I426", "K292", "K700", "K703", "K709", "T51", "Z502", "Z714", "Z721"),
         "Drug abuse" = c(paste0("F", 11:16), "F18", "F19", "Z715", "Z722"),
         "Psychoses" = c("F20", paste0("F", 22:25), "F28", "F29", "F302", "F312", "F315"),
         "Depression" = c("F204", paste0("F", 313:315), "F32", "F33", "F341", "F412", "F432"))

  elixh_resid <-
    list("Renal calculus et al (N2)" = c("N2"),
         "Cystitis et al (N3)" = c("N3"),
         "Disorders of breast (N6)" = c("N6"),
         "Oesophagus, stomach & duodenum (K2)" = c("K2"),
         "Hernia (K4)" = c("K4"),
         "Enteritis/colitis (K5)" = c("K5"),
         "Liver (K7)" = c("K7"),
         "Gallbladder (K8)" = c("K8"))

  elixh_names <- c(names(elixh_icd09), names(elixh_resid))
  elixh_names <-
    data.frame(labels=elixh_names,
               names =c("heartf", "arythm", "valdis", "pulcir", "pervas", "hyperu", "hyperc", "paraly",
                        "oneuro", "copdis", "diabeu", "diabec", "hypthy", "renalf", "liverd", "peptic",
                        "aidshv", "lympho", "metaca", "solica", "rheuma", "coagul", "obesty", "weiglo",
                        "fluide", "bldlos", "anaemi", "alchol", "drugab", "psycho", "depres", "renaN2",
                        "cystN3", "breaN6", "stomK2", "hernK4", "entiK5", "liveK7", "gallK8"
               )
    )
}

# Custom diagnosis classification
custm_icd09 <-
  list(c_neuro = c("430":"434", "436", "290"),
       c_heart = c("42709":"42719", "42899", "78249", "410", "413", "42793", "42794"),
       c_pulmn = c("490", "491", "518"),
       c_liver = c("570", "571", "456"),
       c_renal = c("403", "404", "580":"584", "59009", "59319", "75310":"75319", "792"),
       c_diabt = c("249", "250"),
       c_cancr = c("140":"194")
       #c_immun = c("204":"208")
  )

custm_icd10 <-
  list(c_neuro = c(paste0("I", 60:64), "I69", paste0("F0", "0":"3"), "F1073", "F1173", "F1273", "F1373", "F1473", "F1573", "F1673", "F1873", "F1973"),
       c_heart = c(paste0("I", 20:25), "I099A", "I110", "I130", "I132", "I48", "I50"),
       c_pulmn = c(paste0("J", 41:44)),
       c_liver = c(paste0("K", 700:704), "K709", paste0("K", 71:74), "K76", "K766", "I85"),
       c_renal = c("I12", "I13", paste0("N0", 0:5), "N07", "N08", "N11", "N14", "N18", "N19", "Q61"),
       c_diabt = c("E10", "E11", "E13", "E14"),
       c_cancr = c(paste0("C0", 0:9), paste0("C", 10:43), paste0("C", 45:97))
       #c_immun = c(paste0("C", 81:85), "C88", paste0("C", 90:96))
  )

custm_names <- c(names(custm_icd09))
custm_names <-
  data.frame(labels=custm_names,
             names=c("neuro", "heart", "pulmn", "liver", "renal", "diabt", "cancr"))

# wrap up and save in list
comorbs <- list(charl_icd09=charl_icd09, charl_icd10=charl_icd10, charl_names=charl_names,
                elixh_icd09=elixh_icd09, elixh_icd10=elixh_icd10, elixh_names=elixh_names, elixh_resid=elixh_resid,
                custm_icd09=custm_icd09, custm_icd10=custm_icd10, custm_names=custm_names)

rm(list=c("charl_icd09", "charl_icd10", "charl_names",
          "elixh_icd09", "elixh_icd10", "elixh_names",
          "custm_icd09", "custm_icd10", "custm_names", "elixh_resid"))

usethis::use_data(comorbs, overwrite=T)
