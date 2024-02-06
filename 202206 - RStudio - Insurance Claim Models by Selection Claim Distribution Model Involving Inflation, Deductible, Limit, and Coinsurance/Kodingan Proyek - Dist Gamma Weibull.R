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

Besar_Klaim = sort(data_2020)
plot(Besar_Klaim, main = "Grafik Besaran Klaim")

teta = mean(data_2020)
alpha = (teta^2)/var(data_2020)
beta = var(data_2020)/teta

# ========== Gambar ==========
#Gambar data_2020 + dist.Gamma
dplot(data_2020,"pgamma",list(shape=alpha, scale=beta),H=0,vertical=TRUE)

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


##############################################################################################################################
# ========== [DISTRIBUSI WEIBULL] ========== #
###

# Parameter dist.Weibull dan bangkitkan data sebanyak "jmlh_x_bawah-1" buah dgn syarat ttp <1jt JPY
fitdist(x_atas,"weibull")

set.seed(1)
w = rep(10, jmlh_x_bawah-1)
l = 1
while (TRUE) {
  r = rweibull(jmlh_x_bawah-1, shape=3.052824, scale=2450099)
  if (r > 0 && r < t) {
    w[l] = r
    l = l+1
    if (l>jmlh_x_bawah-1)
      break
  }
}

rdm22_weibull = w

#Combine data > batas t, dan data random < batas t sebanyak "jmlh_x_bawah-1" yg telah dibangkitkan dgn dist.Weibull

data_2021w = append(x_atas,rdm22_weibull)

#Penghilangan Inflasi
data_2020w = data_2021w/i

# Parameter baru untuk dist.Weibull
fitdist(data_2020w,"weibull")

# ========== Gambar ==========
#Gambar data_2020w + dist.Weibull
dplot(data_2020w,"pweibull",list(shape =2.763368,scale = 2337924), H = jmlh_x_bawah-1,vertical=TRUE)

# BUAT QQ PLOT UNTUK DATA xt DENGAN DISTRIBUSI Weibull (teta, tau)
qplot(data_2020w,"pweibull",list(shape =2.763368,scale = 2337924 ),H=0)

# ========== Uji-uji ==========
# KOLMOGOROV SMIRNOV TEST UNTUK DIST. Weibull(teta, tau)
ks.test(data_2020w,"pweibull",list(shape =2.763368,scale = 2337924 ),H=0)
#didapat p-value = 0.7 sangat bagus
#0.7 > dan jauh banget dari 0.05, jadi bisa dibilang H0 tidak ditolak
#Jadi, dist gamma sangat baik untuk menggambarkan data kita

#ANDERSON DARLING TEST UNTUK DIST. Weibull(teta, tau)
ad.test(data_2020w,"pweibull",list(shape =2.763368,scale = 2337924 ),H=0)
#p - value nya = 0.29
#dist. gamma diterima di sini

# CRAMER-VON MISES TEST UNTUK DIST Weibull(teta, tau)
w2.test(data_2020w,"pweibull",list(shape =2.763368,scale = 2337924 ),H=0)
#p -value = 0.4
#0.4 > dan jauh banget dari 0.05, jadi bisa dibilang H0 tidak ditolak dan p-valuenya bagus
#paling oke memang C-V test di sini