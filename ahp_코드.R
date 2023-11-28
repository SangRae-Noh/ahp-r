install.packages("ahpsurvey")
library(ahpsurvey)

#설문 자료 불러오기
survey <- read.csv("survey.csv")
survey = survey[-1]

#계층별로 분할
class1 = survey[1:3]
class21 = survey[4:6]
class22 = survey[7:9]
class23 = survey[10:12]

#계층별 기준
atts1 = c("PP", "F", "C")
atts21 = c("d", "ir", "s")
atts22 = c("b", "i", "gtx")
atts23 = c("qdf", "m", "lw")

#계층별 쌍대비교행렬(PCM)
pcm1 = ahp.mat(df = class1, atts = atts1, negconvert = TRUE)
pcm21 = ahp.mat(df = class21, atts = atts21, negconvert = TRUE)
pcm22 = ahp.mat(df = class22, atts = atts22, negconvert = TRUE)
pcm23 = ahp.mat(df = class23, atts = atts23, negconvert = TRUE)

#개별 가중치 계산
indpref1 = ahp.indpref(pcm1, atts = atts1, method = "arithmetic")
indpref21 = ahp.indpref(pcm21, atts = atts21, method = "arithmetic")
indpref22 = ahp.indpref(pcm22, atts = atts22, method = "arithmetic")
indpref23 = ahp.indpref(pcm23, atts = atts23, method = "arithmetic")

#계층, 기준별 CR
cr1 <- ahp.cr(ahpmat = pcm1,atts = atts1)
cr21 <- ahp.cr(ahpmat = pcm21, atts = atts21)
cr22 <- ahp.cr(ahpmat = pcm22, atts = atts22)
cr23 <- ahp.cr(ahpmat = pcm23, atts = atts23)

#개별 가중치 & CR
pref_cr1 = cbind(indpref1, cr1)
pref_cr21 = cbind(indpref21, cr21)
pref_cr22 = cbind(indpref22, cr22)
pref_cr23 = cbind(indpref23, cr23)

#이 중 CR>0.1 인 행 제거
pref_cr1_censor <- subset(pref_cr1, cr1 <= 0.1)
pref_cr21_censor <- subset(pref_cr21, cr21 <= 0.1)
pref_cr22_censor <- subset(pref_cr22, cr22 <= 0.1)
pref_cr23_censor <- subset(pref_cr23, cr23 <= 0.1)

#남은 응답자들 가중치 기하평균 & 합이 1이 되도록 변환
#계층 1 ..방호성/기능성/착용 편의성
gmean_PP <- prod(pref_cr1_censor$PP)^(1/length(pref_cr1_censor$cr1))
gmean_F <- prod(pref_cr1_censor$F)^(1/length(pref_cr1_censor$cr1))
gmean_C <- prod(pref_cr1_censor$C)^(1/length(pref_cr1_censor$cr1))

weight_PP = gmean_PP/(gmean_PP+gmean_F+gmean_C)
weight_F = gmean_F/(gmean_PP+gmean_F+gmean_C)
weight_C = gmean_C/(gmean_PP+gmean_F+gmean_C)

#계층 2-1 ..내구성/내충격성/안전성
gmean_d <- prod(pref_cr21_censor$d)^(1/length(pref_cr21_censor$cr21))
gmean_ir <- prod(pref_cr21_censor$ir)^(1/length(pref_cr21_censor$cr21))
gmean_s <- prod(pref_cr21_censor$s)^(1/length(pref_cr21_censor$cr21))

weight_d = gmean_d/(gmean_d+gmean_ir+gmean_s)
weight_ir = gmean_ir/(gmean_d+gmean_ir+gmean_s)
weight_s = gmean_s/(gmean_d+gmean_ir+gmean_s)

#계층 2-2 ..통기성/보온성/고어텍스
gmean_b <- prod(pref_cr22_censor$b)^(1/length(pref_cr22_censor$cr22))
gmean_i <- prod(pref_cr22_censor$i)^(1/length(pref_cr22_censor$cr22))
gmean_gtx <- prod(pref_cr22_censor$gtx)^(1/length(pref_cr22_censor$cr22))

weight_b = gmean_b/(gmean_b+gmean_i+gmean_gtx)
weight_i = gmean_i/(gmean_b+gmean_i+gmean_gtx)
weight_gtx = gmean_gtx/(gmean_b+gmean_i+gmean_gtx)

#계층 2-3 ..신속 착용 기능/활동성/경량성
gmean_qdf <- prod(pref_cr23_censor$qdf)^(1/length(pref_cr23_censor$cr23))
gmean_m <- prod(pref_cr23_censor$m)^(1/length(pref_cr23_censor$cr23))
gmean_lw <- prod(pref_cr23_censor$lw)^(1/length(pref_cr23_censor$cr23))

weight_qdf = gmean_qdf/(gmean_qdf+gmean_m+gmean_lw)
weight_m = gmean_m/(gmean_qdf+gmean_m+gmean_lw)
weight_lw = gmean_lw/(gmean_qdf+gmean_m+gmean_lw)

#종합 가중치
aw_d = weight_PP * weight_d
aw_ir = weight_PP * weight_ir
aw_s = weight_PP * weight_s
aw_b = weight_F * weight_b
aw_i = weight_F * weight_i
aw_gtx = weight_F * weight_gtx
aw_qdf = weight_C * weight_qdf
aw_m = weight_C * weight_m
aw_lw = weight_C * weight_lw

종합가중치 = c(aw_d, aw_ir, aw_s, aw_b, aw_i, aw_gtx, aw_qdf, aw_m, aw_lw)

survey2 <- read.csv("survey2.csv")


alt1 = survey2[1]
alt2 = survey2[2]
alt3 = survey2[3]
alt4 = survey2[4]
alt5 = survey2[5]
alt6 = survey2[6]
alt7 = survey2[7]
alt8 = survey2[8]
alt9 = survey2[9]

atts_alt <-c("TREKSTA", "LOWA")

#기준별 대안 쌍대비교 행렬
pcm_d = ahp.mat(df = alt1, atts = atts_alt, negconvert = TRUE)
pcm_ir = ahp.mat(df = alt2, atts = atts_alt, negconvert = TRUE)
pcm_s = ahp.mat(df = alt3, atts = atts_alt, negconvert = TRUE)
pcm_b = ahp.mat(df = alt4, atts = atts_alt, negconvert = TRUE)
pcm_i = ahp.mat(df = alt5, atts = atts_alt, negconvert = TRUE)
pcm_gtx = ahp.mat(df = alt6, atts = atts_alt, negconvert = TRUE)
pcm_qdf = ahp.mat(df = alt7, atts = atts_alt, negconvert = TRUE)
pcm_m = ahp.mat(df = alt8, atts = atts_alt, negconvert = TRUE)
pcm_lw = ahp.mat(df = alt9, atts = atts_alt, negconvert = TRUE)

#기준별 대안 효과도
indpref_d = ahp.indpref(pcm_d, atts = atts_alt, method = "arithmetic")
indpref_ir = ahp.indpref(pcm_ir, atts = atts_alt, method = "arithmetic")
indpref_s = ahp.indpref(pcm_s, atts = atts_alt, method = "arithmetic")
indpref_b = ahp.indpref(pcm_b, atts = atts_alt, method = "arithmetic")
indpref_i = ahp.indpref(pcm_i, atts = atts_alt, method = "arithmetic")
indpref_gtx = ahp.indpref(pcm_gtx, atts = atts_alt, method = "arithmetic")
indpref_qdf = ahp.indpref(pcm_qdf, atts = atts_alt, method = "arithmetic")
indpref_m = ahp.indpref(pcm_m, atts = atts_alt, method = "arithmetic")
indpref_lw = ahp.indpref(pcm_lw, atts = atts_alt, method = "arithmetic")

#응답자 답변 평균(기하평균) & 합이 1이 되도록 변환

#내구성에 대한 대안비교
gmean_alt_d_T<- prod(indpref_d$TREKSTA)^(1/length(indpref_d$TREKSTA))
gmean_alt_d_L<- prod(indpref_d$LOWA)^(1/length(indpref_d$LOWA))
weight_alt_d_T = gmean_alt_d_T/(gmean_alt_d_T + gmean_alt_d_L)
weight_alt_d_L = gmean_alt_d_L/(gmean_alt_d_T + gmean_alt_d_L)
#내충격성에 대한 대안비교
gmean_alt_ir_T<- prod(indpref_ir$TREKSTA)^(1/length(indpref_ir$TREKSTA))
gmean_alt_ir_L<- prod(indpref_ir$LOWA)^(1/length(indpref_ir$LOWA))
weight_alt_ir_T = gmean_alt_ir_T/(gmean_alt_ir_T + gmean_alt_ir_L)
weight_alt_ir_L = gmean_alt_ir_L/(gmean_alt_ir_T + gmean_alt_ir_L)
#안전성에 대한 대안비교
gmean_alt_s_T<- prod(indpref_s$TREKSTA)^(1/length(indpref_s$TREKSTA))
gmean_alt_s_L<- prod(indpref_s$LOWA)^(1/length(indpref_s$LOWA))
weight_alt_s_T = gmean_alt_s_T/(gmean_alt_s_T + gmean_alt_s_L)
weight_alt_s_L = gmean_alt_s_L/(gmean_alt_s_T + gmean_alt_s_L)
#통기성에 대한 대안비교
gmean_alt_b_T<- prod(indpref_b$TREKSTA)^(1/length(indpref_b$TREKSTA))
gmean_alt_b_L<- prod(indpref_b$LOWA)^(1/length(indpref_b$LOWA))
weight_alt_b_T = gmean_alt_b_T/(gmean_alt_b_T + gmean_alt_b_L)
weight_alt_b_L = gmean_alt_b_L/(gmean_alt_b_T + gmean_alt_b_L)
#보온성에 대한 대안비교
gmean_alt_i_T<- prod(indpref_i$TREKSTA)^(1/length(indpref_i$TREKSTA))
gmean_alt_i_L<- prod(indpref_i$LOWA)^(1/length(indpref_i$LOWA))
weight_alt_i_T = gmean_alt_i_T/(gmean_alt_i_T + gmean_alt_i_L)
weight_alt_i_L = gmean_alt_i_L/(gmean_alt_i_T + gmean_alt_i_L)
#고어텍스에 대한 대안비교
gmean_alt_gtx_T<- prod(indpref_gtx$TREKSTA)^(1/length(indpref_gtx$TREKSTA))
gmean_alt_gtx_L<- prod(indpref_gtx$LOWA)^(1/length(indpref_gtx$LOWA))
weight_alt_gtx_T = gmean_alt_gtx_T/(gmean_alt_gtx_T + gmean_alt_gtx_L)
weight_alt_gtx_L = gmean_alt_gtx_L/(gmean_alt_gtx_T + gmean_alt_gtx_L)
#신속 착용 기능에 대한 대안비교
gmean_alt_qdf_T<- prod(indpref_qdf$TREKSTA)^(1/length(indpref_qdf$TREKSTA))
gmean_alt_qdf_L<- prod(indpref_qdf$LOWA)^(1/length(indpref_qdf$LOWA))
weight_alt_qdf_T = gmean_alt_qdf_T/(gmean_alt_qdf_T + gmean_alt_qdf_L)
weight_alt_qdf_L = gmean_alt_qdf_L/(gmean_alt_qdf_T + gmean_alt_qdf_L)
#활동성에 대한 대안비교
gmean_alt_m_T<- prod(indpref_m$TREKSTA)^(1/length(indpref_m$TREKSTA))
gmean_alt_m_L<- prod(indpref_m$LOWA)^(1/length(indpref_m$LOWA))
weight_alt_m_T = gmean_alt_m_T/(gmean_alt_m_T + gmean_alt_m_L)
weight_alt_m_L = gmean_alt_m_L/(gmean_alt_m_T + gmean_alt_m_L)
#경량성에 대한 대안비교
gmean_alt_lw_T<- prod(indpref_lw$TREKSTA)^(1/length(indpref_lw$TREKSTA))
gmean_alt_lw_L<- prod(indpref_lw$LOWA)^(1/length(indpref_lw$LOWA))
weight_alt_lw_T = gmean_alt_lw_T/(gmean_alt_lw_T + gmean_alt_lw_L)
weight_alt_lw_L = gmean_alt_lw_L/(gmean_alt_lw_T + gmean_alt_lw_L)

대안1_보급_효과도 = c(weight_alt_d_T,weight_alt_ir_T,weight_alt_s_T,weight_alt_b_T,weight_alt_i_T,weight_alt_gtx_T,weight_alt_qdf_T,weight_alt_m_T,weight_alt_lw_T)
대안2_사제_효과도 = c(weight_alt_d_L,weight_alt_ir_L,weight_alt_s_L,weight_alt_b_L,weight_alt_i_L,weight_alt_gtx_L,weight_alt_qdf_L,weight_alt_m_L,weight_alt_lw_L)

기준 = c("내구성", "내충격성", "안전성", "통기성", "보온성", "고어텍스","신속 착용 기능", "활동성", "경량성")
대안_평가_결과 = data.frame(기준, 종합가중치, 대안1_보급_효과도, 대안2_사제_효과도)

대안_평가_결과$대안1_종합_평가치 = 대안_평가_결과$종합가중치 * 대안1_보급_효과도
대안_평가_결과$대안2_종합_평가치 = 대안_평가_결과$종합가중치 * 대안2_사제_효과도
