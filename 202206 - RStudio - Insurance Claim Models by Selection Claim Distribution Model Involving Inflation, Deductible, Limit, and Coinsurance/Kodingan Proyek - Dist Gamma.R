install.packages(truncgof)
install.packages("truncgof",
                 repos="https://mran.microsoft.com/snapshot/2022-05-08")
install.packages("fitdistrplus")
library(MASS)
library(truncgof)
library(fitdistrplus)

# ========== Pembersihan Data (Kecuali Inflasi) ==========

#Data
data <- project

#Pilih nilai Coinsurance (c)
c = 0.7
#Pilih batas pengurangan 1jt JPY (Deductible)
t = 1000000

#Nilai data yang baru
bersih_c = data/c
bersih_d = bersih_c+t

# ========== Pembagian Data u/ bangkitin rand.var ber dist ==========

#Pembagian data >1jt JPY dan <1jt JPY
x_atas = bersih_d[bersih_d>t]#data2 yg >1jt
teta_atas = mean(x_atas)

x_bawah = bersih_d[bersih_d<=t] #data2 yg <1jt
jmlh_x_bawah = sum(bersih_d <= t, na.tm=TRUE) #jmlh data2 yg <1jt, kepake u/ nentuin byk rand.var yg mau di bangkitkan


##############################################################################################################################
# ========== [DISTRIBUSI GAMMA] ========== #
###

#Parameter dist. Gamma, dan bangkitkan data sebanyak "jmlh_x_bawah-1" buah dgn syarat ttp <1jt JPY
alpha_atas = (teta_atas^2)/var(x_atas)
beta_atas = var(x_atas)/teta_atas

set.seed(1)

v = rep(1000000, jmlh_x_bawah-1)
k = 1
while (TRUE) {
  q = rgamma(jmlh_x_bawah-1, shape=alpha_atas, scale=beta_atas)
  if (q > 0 && q < t) {
    v[k] = q
    k = k+1
    if (k>jmlh_x_bawah-1)
      break
  }
}

rdm22_gamma = v #ini udh bener sesuai jmlh_x_bawah-1 yg mau dibangkitin, dan sudah sesuai batas rand.var nya harus <1jt
teta_bawah = mean(rdm22_gamma) #ini kepake nanti buat nyari teta gabungan, kalo ternyata var x_atas & rdm_gamma gak bisa di gabung.

#Combine data > batas t, dan data random < batas t sebanyak "jmlh_x_bawah-1" yg telah dibangkitkan dgn dist.Gamma

data_2021 = append(x_atas,rdm22_gamma)

#Penghilangan Inflasi
i = 1.01
data_2020 = data_2021/i

teta = mean(data_2020)
alpha = (teta^2)/var(data_2020)
beta = var(data_2020)/teta

# ========== Gambar ==========
#Gambar data_2020 + dist.Gamma
#dplot(data_2020,"pgamma",list(shape=alpha, scale=beta),H=0,vertical=TRUE)

# BUAT QQ PLOT UNTUK DATA xt DENGAN DISTRIBUSI Gamma (alfa, beta)
qplot(data_2020,"pgamma",list(shape=alpha, scale=beta),H=0)

# ========== Uji-uji ==========
# KOLMOGOROV SMIRNOV TEST UNTUK DIST. GAMMA(a,b)
ks.test(data_2020,"pgamma",list(shape=alpha, scale=beta),H=0)
#didapat p-value = 0.78 sangat bagus
#0.78 > dan jauh banget dari 0.05, jadi bisa dibilang H0 tidak ditolak
#Jadi, dist gamma sangat baik untuk menggambarkan data kita

#ANDERSON DARLING TEST UNTUK DIST. GAMMA(a,b)
ad.test(data_2020,"pgamma",list(shape=alpha, scale=beta),H=0)
#p - value nya = 1
#dist. gamma diterima di sini

# CRAMER-VON MISES TEST UNTUK DIST GAMMA(a,b)
w2.test(data_2020,"pgamma",list(shape=alpha, scale=beta),H=0)
#p -value = 0.88
#0.88 > dan jauh banget dari 0.05, jadi bisa dibilang H0 tidak ditolak dan p-valuenya bagus
#paling oke memang C-V test di sini

