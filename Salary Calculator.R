
# Calculates, Prints Tax and Salary (Philippines 2023)

your_salary <- function(n = 1) {
  
  # Looper
  x <- 1
  
  # Tax tiers 2023 withholding table
  
  t_1 <- 20833 # tier 1 of tax
  t_2 <- 33332 # tier 2 of tax
  t_3 <- 66667 # tier 3 of tax
  t_4 <- 166666 # tier 4 of tax
  t_5 <- 666666 # tier 5 of tax
  
  # SSS tiers 2023
  
  s1 <- 1000
  s2 <- 3250
  s3 <- s2 + 500
  s4 <- s3 + 500
  s5 <- s4 + 500
  s6 <- s5 + 500
  s7 <- s6 + 500
  s8 <- s7 + 500
  s9 <- s8 + 500
  s10 <- s9 + 500
  s11 <- s10 + 500
  s12 <- s11 + 500
  s13 <- s12 + 500
  s14 <- s13 + 500
  s15 <- s14 + 500
  s16 <- s15 + 500
  s17 <- s16 + 500
  s18 <- s17 + 500
  s19 <- s18 + 500
  s20 <- s19 + 500
  s21 <- s20 + 500
  s22 <- s21 + 500
  s23 <- s22 + 500
  s24 <- s23 + 500
  s25 <- s24 + 500
  s26 <- s25 + 500
  s27 <- s26 + 500
  s28 <- s27 + 500
  s29 <- s28 + 500
  s30 <- s29 + 500
  s31 <- s30 + 500
  s32 <- s31 + 500
  s33 <- s32 + 500
  s34 <- s33 + 500
  s35 <- s34 + 500
  s36 <- s35 + 500
  s37 <- s36 + 500
  s38 <- s37 + 500
  s39 <- s38 + 500
  s40 <- s39 + 500
  s41 <- s40 + 500
  s42 <- s41 + 500
  s43 <- s42 + 500
  s44 <- s43 + 500 
  s45 <- s44 + 500 #24750
  
  
  # Asks for monthly salary
  
  while(x <= n) {
    x <- as.double(readline(prompt = "Enter your monthly salary: "))
    
    
    # --- salary calculators
    
    daily_salary <- x / 24.08
    hourly_salary <- daily_salary / 8.5
    monthly_salary <- x
    annual_salary <- x * 13
    
    
    
    # < ----------------------------------------- All deductions
    
    ## --- withholding tax calculator
    
    ### --- if monthly salary is equal or below 20833
    if (x > 0 & x <= t_1) {
      tax <- 0
      
      ### --- if monthly salary is below 33332 but above 20833 
    } else if (x > t_1 & x < t_2 ) {
      tax <- (x - t_1) * 0.15
      
      ### --- if monthly salary is below 66666 but above 33332
    } else if (x > t_2 & x < t_3 ) {
      tax <- ((x - t_2) * 0.20) + 1875
      
      ### --- if monthly salary is below 166666 but above 66666  
    } else if (x > t_3 & x < t_4) {
      tax <- ((x - t_3) * 0.25) + 8541.8
      
      ### --- if monthly salary is below 666666 but above 166666  
    } else if (x > t_4 & x < t_5) {
      tax <- ((x - t_4) * 0.30) + 33541.8
      
      ### --- if monthly salary is above 666666
    } else {
      tax <- ((x - t_5) * 0.35) + 183541.80
    }
    
    
    # --- calculates sss contribution (--THIS IS NOT YET FINAL--)
    
    sss_contribution <- 0
    
    if (x >= s1 & x < s2) {
      sss_contribution <- 135
      
    } else if (x >= s2 & x < s3) {
      sss_contribution <- 157
      
    } else if ( x >= s3 & x < s4) {
      sss_contribution <- 180
      
    } else if ( x >= s4 & x < s5 ) {
      sss_contribution <- 202.50
      
    } else if ( x >= s5 & x < s6 ) {
      sss_contribution <- 225
      
    } else if ( x >= s6 & x < s7 ) {
      sss_contribution <- 247.50
      
    } else if ( x >= s7 & x < s8 ) {
      sss_contribution <- 270
      
    } else if ( x >= s8 & x < s9 ) {
      sss_contribution <- 292.50
      
    } else if ( x >= s9 & x < s10) {
      sss_contribution <- 315
      
    } else if ( x >= s10 & x < s11) {
      sss_contribution <- 337.50
      
    } else if ( x >= s11 & x < s12 ) {
      sss_contribution <- 360
      
    } else if ( x >= s12 & x < s13 ) {
      sss_contribution <- 382.50
      
    } else if ( x >= s13 & x < s14 ) {
      sss_contribution <- 405
      
    } else if ( x >= s14 & x < s15 ) {
      sss_contribution <- 427.50
      
    } else if ( x >= s15 & x < s16 ) {
      sss_contribution <- 450
      
    } else if ( x >= s16 & x < s17 ) {
      sss_contribution <- 472.50
      
    } else if ( x >= s17 & x < s18 ) {
      sss_contribution <- 495
      
    } else if ( x >= s18 & x < s19 ) {
      sss_contribution <- 517.50
      
    } else if ( x >= s19 & x < s20 ) {
      sss_contribution <- 540
      
    } else if ( x >= s20 & x < s21 ) {
      sss_contribution <- 562.50
      
    } else if ( x >= s21 & x < s22 ) {
      sss_contribution <- 585
      
    } else if ( x >= s22 & x < s23 ) {
      sss_contribution <- 607.50
      
    } else if ( x >= s23 & x < s24 ) {
      sss_contribution <- 630
      
    } else if ( x >= s24 & x < s25 ) {
      sss_contribution <- 675
      
    } else if ( x >= s25 & x < s26 ) {
      sss_contribution <- 697.50
      
    } else if ( x >= s26 & x < s27 ) {
      sss_contribution <- 720
      
    } else if ( x >= s27 & x < s28 ) {
      sss_contribution <- 742.50
      
    } else if ( x >= s28 & x < s29 ) {
      sss_contribution <- 765
      
    } else if ( x >= s29 & x < s30 ) {
      sss_contribution <- 787.50
      
    } else if ( x >= s30 & x < s31 ) {
      sss_contribution <- 810
      
    } else if ( x >= s31 & x < s32 ) {
      sss_contribution <- 832.50
      
    } else if ( x >= s32 & x < s33 ) {
      sss_contribution <- 855
      
    } else if ( x >= s33 & x < s34 ) {
      sss_contribution <- 877.50
      
    } else if ( x >= s34 & x < s35 ) {
      sss_contribution <- 900
      
    } else if ( x >= s35 & x < s36 ) {
      sss_contribution <- 922.50
      
    } else if ( x >= s36 & x < s37 ) {
      sss_contribution <- 945
      
    } else if ( x >= s37 & x < s38 ) {
      sss_contribution <- 967.50
      
    } else if ( x >= s38 & x < s39 ) {
      sss_contribution <- 990
      
    } else if ( x >= s39 & x < s40 ) {
      sss_contribution <- 1012.50
      
    } else if ( x >= s40 & x < s41 ) {
      sss_contribution <- 1035
      
    } else if ( x >= s41 & x < s42 ) {
      sss_contribution <- 1057.50
      
    } else if ( x >= s42 & x < s43 ) {
      sss_contribution <- 1080
      
    } else if ( x >= s43 & x < s44 ) {
      sss_contribution <- 1102.50
      
    } else if ( x >= s44) {
      sss_contribution <- 1125
      
    } else {
      sss_contribution <- 0
      
    }
    
    
    
    
    # --- calculates pagibig contribution
    
    pagibig_contribution <- x * 0.002
    
    # --- calculates philhealth contribution
    
    philhealth_contribution <- (x * 0.04) / 2
    
    # Sums up all deductions
    all_deductions <- sss_contribution + pagibig_contribution + philhealth_contribution + tax
    
    
    # --- take home pay calculator
    
    net_pay <- monthly_salary - all_deductions
    
    # --- pay per cutoff
    
    cutoff_pay <- net_pay / 2
    
    # Matrix for salary report
    
    
    salary_report <- matrix(
      
      # Taking the sequence of elements
      c(annual_salary,
        daily_salary,
        hourly_salary,
        cutoff_pay,
        monthly_salary,
        net_pay,
        sss_contribution,
        tax,
        pagibig_contribution,
        philhealth_contribution),
      
      # No of rows
      nrow = 10,
      
      # No of columns
      ncol = 1,
      
      
      # By default matrices are in column-wise order
      byrow = TRUE
    )
    
    # Naming rows
    rownames(salary_report) = c("annual_salary",
                                "daily_salary",
                                "hourly_salary",
                                "cutoff_pay",
                                "monthly_salary",
                                "net_pay",
                                "sss_contribution",
                                "tax",
                                "pagibig_contribution",
                                "philhealth_contribution")
    colnames(salary_report) = c("Peso value")
  }
  View(salary_report)
}


