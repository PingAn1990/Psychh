#' spec2 configuration (plain; GitHub-ready)
#' @export
spec2 <- function() {
  list(
    meta = list(
      spec_id = "spec2",
      paper_id = "统计.docx",
      N_reported = 832L,
      likert_range = c(1, 7)
    ),
    
    schema = list(
      demographics = c("Gender","MaritalStatus","Education","AgeGroup","IncomeGroup","Residence"),
      items_by_construct = list(
        ENK = c("ENK1","ENK2","ENK3","ENK4"),
        ENA = c("ENA1","ENA2","ENA3","ENA4","ENA5"),
        ENR = c("ENR1","ENR2","ENR3"),
        PRC = c("PRC1","PRC2"),
        GRP = c("GRP1","GRP2"),
        FCL = c("FCL1","FCL2","FCL3","FCL4","FCL5"),
        GRC = c("GRC1","GRC2","GRC3")
      ),
      construct_order = c("ENK","ENA","ENR","PRC","GRP","FCL","GRC"),
      dv = "GRC",
      predictors_main = c("ENK","ENA","ENR"),
      moderators = c("PRC","GRP","FCL")
    ),
    
    match = list(
      require_exact_items = TRUE,
      construct_alias = list(
        "Perceived Cost" = "PRC",
        "Perceived Costs" = "PRC",
        "Policy Incentive" = "GRP",
        "Government Policy" = "GRP",
        "Face Culture" = "FCL",
        "Green Consumption" = "GRC",
        "Environmental Knowledge" = "ENK",
        "Environmental Affection" = "ENA",
        "Environmental Responsibility" = "ENR"
      )
    ),
    
    reported = list(
      table2 = list(
        ENK = list(
          loadings = c(ENK1 = 0.88328788, ENK2 = 0.8097581, ENK3 = 0.79140035, ENK4 = 0.84937616),
          CR = 0.92400654,
          CA = 0.84752425,
          KMO = 0.78534599,
          bartlett = list(chi2 = 1539, df = 5, Sig = 0.00022722)
        ),
        ENA = list(
          loadings = c(ENA1 = 0.78713891, ENA2 = 0.71691106, ENA3 = 0.80919975, ENA4 = 0.77161332, ENA5 = 0.69136276),
          CR = 0.87560605,
          CA = 0.79116084,
          KMO = 0.83375605,
          bartlett = list(chi2 = 1485, df = 9, Sig = 0.00029145)
        ),
        ENR = list(
          loadings = c(ENR1 = 0.66124146, ENR2 = 0.69429601, ENR3 = 0.71611895),
          CR = 0.82037704,
          CA = 0.81149332,
          KMO = 0.77422444,
          bartlett = list(chi2 = 637, df = 4, Sig = 0.00975646)
        ),
        PRC = list(
          loadings = c(PRC1 = 0.85427451, PRC2 = 0.85597552),
          CR = 0.84568258,
          CA = 0.78155534,
          KMO = 0.82821848,
          bartlett = list(chi2 = 436, df = 3, Sig = 0.00029082)
        ),
        GRP = list(
          loadings = c(GRP1 = 0.63020475, GRP2 = 0.70110951),
          CR = 0.89875548,
          CA = 0.74905104,
          KMO = 0.80983627,
          bartlett = list(chi2 = 588, df = 2, Sig = 0.00046498)
        ),
        FCL = list(
          loadings = c(FCL1 = 0.81854013, FCL2 = 0.81053621, FCL3 = 0.7258979, FCL4 = 0.78698942, FCL5 = 0.76697679),
          CR = 0.86548522,
          CA = 0.8206808,
          KMO = 0.76564984,
          bartlett = list(chi2 = 613, df = 4, Sig = 0.00996292)
        ),
        GRC = list(
          loadings = c(GRC1 = 0.89074155, GRC2 = 0.82397079, GRC3 = 0.75978973),
          CR = 0.84399072,
          CA = 0.82575277,
          KMO = 0.79844974,
          bartlett = list(chi2 = 746, df = 5, Sig = 0.00045836)
        )
      ),
      
      table3 = local({
        ord <- c("ENK", "ENA", "ENR", "PRC", "GRP", "FCL", "GRC")
        dn  <- list(ord, ord)
        
        v <- c(
          0.87949753, 0.44923928, 0.61661092, -0.50618238, 0.60278952, 0.52403928, 0.43415306,
          0.44924024, 0.85337152, 0.63359566, -0.41514343, 0.40036219, 0.39139298, 0.41014946,
          0.62037316, 0.62849839, 0.85721642, -0.58801459, 0.55287794, 0.52396581, 0.48743617,
          -0.51152792, -0.42116068, -0.59092012, 0.85107853, -0.48944190, -0.39717895, -0.42782945,
          0.60259454, 0.39546789, 0.54786433, -0.49445821, 0.77791847, -0.68003186, -0.41921232,
          0.52140373, 0.38572640, 0.52069055, -0.40166226, -0.68041580, 0.90447967, 0.52966599,
          0.43148078, 0.40613849, 0.48561871, -0.43342593, -0.42393491, 0.53366410, 0.84792342
        )
        
        M <- matrix(v, nrow = 7, byrow = TRUE, dimnames = dn)
        
        list(
          order      = ord,
          matrix_raw = matrix(sprintf("%.8f", v), nrow = 7, byrow = TRUE, dimnames = dn),
          sanitize   = list(),   # par() 会遍历 names(sanitize)，空列表是安全的
          note       = "",
          matrix     = M         # 可选保留：给其他地方直接用数值矩阵
        )
      }),
      
      
      
      table4 = list(
        I = list(
          coef = c(ENK = 0.30717721, ENA = 0.31949897, ENR = 0.34069564),
          t = c(ENK = 4.58063027, ENA = 5.8675897, ENR = 6.22630065),
          p = c(ENK = 0.00377738, ENA = 0.000975, ENR = 0.00446072),
          adj_r2 = 0.38623987,
          F = 112.61949533,
          Sig = 0.00022756
        ),
        II = list(
          coef = c(ENK = 0.32485665, ENA = 0.25861218, ENR = 0.27781192, `ENK:ENA` = 0.00009988, `ENK:ENR` = -0.01881359, `ENA:ENR` = 0.08793158),
          t = c(ENK = 4.98216928, ENA = 4.81520462, ENR = 5.38123632, `ENK:ENA` = 0.11395684, `ENK:ENR` = -0.39366855, `ENA:ENR` = 0.98356571),
          p = c(ENK = 0.00349443, ENA = 0.00356817, ENR = 0.00274902, `ENK:ENA` = 0.81282195, `ENK:ENR` = 0.71375079, `ENA:ENR` = 0.00067206),
          adj_r2 = 0.41166466,
          F = 63.25407248,
          Sig = 0.00044118
        )
      ),
      
      table5 = list(
        PRC = list(
          I = list(
            coef = c(ENK = 0.31999756, ENA = 0.33025437, ENR = 0.34000354),
            stars = c(ENK = "***", ENA = "***", ENR = "***"),
            adj_r2 = 0.42132731,
            F = 112.33581877,
            Sig = 0.00013122
          ),
          II = list(
            coef = c(ENK = 0.21053565, ENA = 0.21338594, ENR = 0.20083671, PRC = -0.41751286),
            stars = c(ENK = "***", ENA = "***", ENR = "**", PRC = "***"),
            adj_r2 = 0.45525359,
            F = 134.2508631,
            Sig = 0.00025609
          ),
          III = list(
            coef = c(ENK = 0.11710147, ENA = 0.19380467, ENR = 0.12300146, PRC = -0.40705723, `ENK:PRC` = 0.0504055, `ENA:PRC` = -0.09339353, `ENR:PRC` = 0.0261931),
            stars = c(ENK = "**", ENA = "***", ENR = "**", PRC = "***", `ENK:PRC` = "", `ENA:PRC` = "**", `ENR:PRC` = ""),
            adj_r2 = 0.45097392,
            F = 87.21991135,
            Sig = 0.00049882
          )
        ),
        GRP = list(
          I = list(
            coef = c(ENK = 0.31072244, ENA = 0.32574588, ENR = 0.33682668),
            stars = c(ENK = "***", ENA = "***", ENR = "***"),
            adj_r2 = 0.39681444,
            F = 125.32378195,
            Sig = 0.00000329
          ),
          II = list(
            coef = c(ENK = 0.30443218, ENA = 0.32324048, ENR = 0.31977081, GRP = 0.05334763),
            stars = c(ENK = "***", ENA = "***", ENR = "***", GRP = ""),
            adj_r2 = 0.40369517,
            F = 92.36626001,
            Sig = 0.00027952
          ),
          III = list(
            coef = c(ENK = 0.20973529, ENA = 0.30039894, ENR = 0.32152485, GRP = 0.0382061, `ENK:GRP` = -0.00077946, `ENA:GRP` = 0.02787955, `ENR:GRP` = -0.03952869),
            stars = c(ENK = "***", ENA = "**", ENR = "***", GRP = "", `ENK:GRP` = "", `ENA:GRP` = "", `ENR:GRP` = ""),
            adj_r2 = 0.40515943,
            F = 46.38009642,
            Sig = 0.00047975
          )
        ),
        FCL = list(
          I = list(
            coef = c(ENK = 0.32455284, ENA = 0.31694374, ENR = 0.33974418),
            stars = c(ENK = "***", ENA = "***", ENR = "***"),
            adj_r2 = 0.43133994,
            F = 128.37264659,
            Sig = 0.0001833
          ),
          II = list(
            coef = c(ENK = 0.21085514, ENA = 0.29636317, ENR = 0.21351825, FCL = 0.31356296),
            stars = c(ENK = "***", ENA = "***", ENR = "***", FCL = "***"),
            adj_r2 = 0.41126926,
            F = 63.34210073,
            Sig = 0.00007593
          ),
          III = list(
            coef = c(ENK = 0.19087183, ENA = 0.18304792, ENR = 0.19863316, FCL = 0.31653463, `ENK:FCL` = 0.00488538, `ENA:FCL` = 0.06593154, `ENR:FCL` = 0.0388504),
            stars = c(ENK = "***", ENA = "***", ENR = "***", FCL = "***", `ENK:FCL` = "", `ENA:FCL` = "*", `ENR:FCL` = ""),
            adj_r2 = 0.39682506,
            F = 70.58476445,
            Sig = 0.00000347
          )
        )
      )
    ),
    
    model_registry = list(
      table4_I = list(
        table = "table4",
        model = "I",
        dv = "GRC",
        main = c("ENK", "ENA", "ENR"),
        interactions = character(0),
        panel = NA
      ),
      table4_II = list(
        table = "table4",
        model = "II",
        dv = "GRC",
        main = c("ENK", "ENA", "ENR"),
        interactions = c("ENK:ENA", "ENK:ENR", "ENA:ENR"),
        panel = NA
      ),
      table5_FCL_I = list(
        table = "table5",
        model = "I",
        dv = "GRC",
        main = c("ENK", "ENA", "ENR"),
        interactions = character(0),
        panel = "FCL"
      ),
      table5_FCL_II = list(
        table = "table5",
        model = "II",
        dv = "GRC",
        main = c("ENK", "ENA", "ENR", "FCL"),
        interactions = character(0),
        panel = "FCL"
      ),
      table5_FCL_III = list(
        table = "table5",
        model = "III",
        dv = "GRC",
        main = c("ENK", "ENA", "ENR", "FCL"),
        interactions = c("ENK:FCL", "ENA:FCL", "ENR:FCL"),
        panel = "FCL"
      ),
      table5_GRP_I = list(
        table = "table5",
        model = "I",
        dv = "GRC",
        main = c("ENK", "ENA", "ENR"),
        interactions = character(0),
        panel = "GRP"
      ),
      table5_GRP_II = list(
        table = "table5",
        model = "II",
        dv = "GRC",
        main = c("ENK", "ENA", "ENR", "GRP"),
        interactions = character(0),
        panel = "GRP"
      ),
      table5_GRP_III = list(
        table = "table5",
        model = "III",
        dv = "GRC",
        main = c("ENK", "ENA", "ENR", "GRP"),
        interactions = c("ENK:GRP", "ENA:GRP", "ENR:GRP"),
        panel = "GRP"
      ),
      table5_PRC_I = list(
        table = "table5",
        model = "I",
        dv = "GRC",
        main = c("ENK", "ENA", "ENR"),
        interactions = character(0),
        panel = "PRC"
      ),
      table5_PRC_II = list(
        table = "table5",
        model = "II",
        dv = "GRC",
        main = c("ENK", "ENA", "ENR", "PRC"),
        interactions = character(0),
        panel = "PRC"
      ),
      table5_PRC_III = list(
        table = "table5",
        model = "III",
        dv = "GRC",
        main = c("ENK", "ENA", "ENR", "PRC"),
        interactions = c("ENK:PRC", "ENA:PRC", "ENR:PRC"),
        panel = "PRC"
      )
    ),
    
    print = list(
      digits_beta = 2,
      digits_loading = 2,
      digits_r2 = 2,
      digits_F = 2,
      p_digits = 3
    )
  )
}
