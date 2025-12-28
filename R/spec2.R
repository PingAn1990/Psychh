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
        loadings = c(ENK1 = 0.88, ENK2 = 0.81, ENK3 = 0.79, ENK4 = 0.85),
        CR = 0.92,
        CA = 0.85,
        KMO = 0.79,
        bartlett = list(chi2 = 1539, df = 5, Sig = 0.0)
      ),
      ENA = list(
        loadings = c(ENA1 = 0.79, ENA2 = 0.72, ENA3 = 0.81, ENA4 = 0.77, ENA5 = 0.69),
        CR = 0.88,
        CA = 0.79,
        KMO = 0.83,
        bartlett = list(chi2 = 1485, df = 9, Sig = 0.0)
      ),
      ENR = list(
        loadings = c(ENR1 = 0.66, ENR2 = 0.69, ENR3 = 0.72),
        CR = 0.82,
        CA = 0.81,
        KMO = 0.77,
        bartlett = list(chi2 = 637, df = 4, Sig = 0.01)
      ),
      PRC = list(
        loadings = c(PRC1 = 0.85, PRC2 = 0.86),
        CR = 0.85,
        CA = 0.78,
        KMO = 0.83,
        bartlett = list(chi2 = 436, df = 3, Sig = 0.0)
      ),
      GRP = list(
        loadings = c(GRP1 = 0.63, GRP2 = 0.7),
        CR = 0.9,
        CA = 0.75,
        KMO = 0.81,
        bartlett = list(chi2 = 588, df = 2, Sig = 0.0)
      ),
      FCL = list(
        loadings = c(FCL1 = 0.82, FCL2 = 0.81, FCL3 = 0.73, FCL4 = 0.79, FCL5 = 0.77),
        CR = 0.87,
        CA = 0.82,
        KMO = 0.77,
        bartlett = list(chi2 = 613, df = 4, Sig = 0.01)
      ),
      GRC = list(
        loadings = c(GRC1 = 0.89, GRC2 = 0.82, GRC3 = 0.76),
        CR = 0.84,
        CA = 0.83,
        KMO = 0.8,
        bartlett = list(chi2 = 746, df = 5, Sig = 0.0)
      )
      ),

      table3 = list(
        matrix = matrix(c(0.88, 0.45, 0.62, -0.51, 0.60, 0.52, 0.43, 0.45, 0.85, 0.63, -0.42, 0.40, 0.39, 0.41, 0.62, 0.63, 0.86, -0.59, 0.55, 0.52, 0.49, -0.51, -0.42, -0.59, 0.85, -0.49, -0.40, -0.43, 0.60, 0.40, 0.55, -0.49, 0.78, -0.68, -0.42, 0.52, 0.39, 0.52, -0.40, -0.68, 0.90, 0.53, 0.43, 0.41, 0.49, -0.43, -0.42, 0.53, 0.85), nrow=7, byrow=TRUE, dimnames=list(c("ENK", "ENA", "ENR", "PRC", "GRP", "FCL", "GRC"), c("ENK", "ENA", "ENR", "PRC", "GRP", "FCL", "GRC")))
      ),

      table4 = list(
        I = list(
          coef = c(ENK = 0.31, ENA = 0.32, ENR = 0.34),
          t = c(ENK = 4.58, ENA = 5.87, ENR = 6.23),
          p = c(ENK = 0.001, ENA = 0.0, ENR = 0.002),
          adj_r2 = 0.39,
          F = 112.62,
          Sig = 0.0
        ),
        II = list(
          coef = c(ENK = 0.32, ENA = 0.26, ENR = 0.28, `ENK:ENA` = 0.005, `ENK:ENR` = -0.021, `ENA:ENR` = 0.086),
          t = c(ENK = 4.98, ENA = 4.82, ENR = 5.38, `ENK:ENA` = 0.11, `ENK:ENR` = -0.39, `ENA:ENR` = 0.98),
          p = c(ENK = 0.0, ENA = 0.001, ENR = 0.0, `ENK:ENA` = 0.812, `ENK:ENR` = 0.711, `ENA:ENR` = 0.005),
          adj_r2 = 0.41,
          F = 63.25,
          Sig = 0.0
        )
      ),

      table5 = list(
      PRC = list(
        I = list(
          coef = c(ENK = 0.32, ENA = 0.33, ENR = 0.34),
          stars = c(ENK = "***", ENA = "***", ENR = "***"),
          adj_r2 = 0.42,
          F = 112.34,
          Sig = 0.0
        ),
        II = list(
          coef = c(ENK = 0.21, ENA = 0.21, ENR = 0.2, PRC = -0.42),
          stars = c(ENK = "***", ENA = "***", ENR = "**", PRC = "***"),
          adj_r2 = 0.46,
          F = 134.25,
          Sig = 0.0
        ),
        III = list(
          coef = c(ENK = 0.12, ENA = 0.19, ENR = 0.12, PRC = -0.41, `ENK:PRC` = 0.05, `ENA:PRC` = -0.09, `ENR:PRC` = 0.03),
          stars = c(ENK = "**", ENA = "***", ENR = "**", PRC = "***", `ENK:PRC` = "", `ENA:PRC` = "**", `ENR:PRC` = ""),
          adj_r2 = 0.45,
          F = 87.22,
          Sig = 0.0
        )
      ),
      GRP = list(
        I = list(
          coef = c(ENK = 0.31, ENA = 0.33, ENR = 0.34),
          stars = c(ENK = "***", ENA = "***", ENR = "***"),
          adj_r2 = 0.4,
          F = 125.32,
          Sig = 0.0
        ),
        II = list(
          coef = c(ENK = 0.3, ENA = 0.32, ENR = 0.32, GRP = 0.05),
          stars = c(ENK = "***", ENA = "***", ENR = "***", GRP = ""),
          adj_r2 = 0.4,
          F = 92.37,
          Sig = 0.0
        ),
        III = list(
          coef = c(ENK = 0.21, ENA = 0.3, ENR = 0.32, GRP = 0.04, `ENK:GRP` = -0.0, `ENA:GRP` = 0.03, `ENR:GRP` = -0.04),
          stars = c(ENK = "***", ENA = "**", ENR = "***", GRP = "", `ENK:GRP` = "", `ENA:GRP` = "", `ENR:GRP` = ""),
          adj_r2 = 0.41,
          F = 46.38,
          Sig = 0.0
        )
      ),
      FCL = list(
        I = list(
          coef = c(ENK = 0.32, ENA = 0.32, ENR = 0.34),
          stars = c(ENK = "***", ENA = "***", ENR = "***"),
          adj_r2 = 0.43,
          F = 128.37,
          Sig = 0.0
        ),
        II = list(
          coef = c(ENK = 0.21, ENA = 0.3, ENR = 0.21, FCL = 0.31),
          stars = c(ENK = "***", ENA = "***", ENR = "***", FCL = "***"),
          adj_r2 = 0.41,
          F = 63.34,
          Sig = 0.0
        ),
        III = list(
          coef = c(ENK = 0.19, ENA = 0.18, ENR = 0.2, FCL = 0.32, `ENK:FCL` = -0.0, `ENA:FCL` = 0.07, `ENR:FCL` = 0.04),
          stars = c(ENK = "***", ENA = "***", ENR = "***", FCL = "***", `ENK:FCL` = "", `ENA:FCL` = "*", `ENR:FCL` = ""),
          adj_r2 = 0.4,
          F = 70.58,
          Sig = 0.0
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
