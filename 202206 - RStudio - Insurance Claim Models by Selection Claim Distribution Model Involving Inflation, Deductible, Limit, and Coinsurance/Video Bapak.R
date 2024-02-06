install.packages(truncgof)
install.packages("truncgof",
                 repos="https://mran.microsoft.com/snapshot/2022-05-08")
library(MASS)
library(truncgof)

set.seed(100)
#truncation pilih 5
t=5
#rgamma artinya membangkitkan data dari dist. gamma sebanyak 100 buah
#kalau pakai R, jika Weibull, cek parameternya.Kalau Normal:parameternya mean dan standar deviasi
#di dist. gamma ada yg parameter beta jadi pembilang atau penyebut
#jadi, cek supaya tidak salah
#di sini shape adalah alpha=3
#di sini, scale adalah beta=7
#jadi, meannya 3x7=21
x = rgamma(100,shape=3,scale=7)
x

#dilakukan pemancungan/truncation
#semua data yg nilainya lebih kecil dari 5 akan dieliminasi
#sehingga ada 3 data dibuang dan 97 data tersisa
xt = x[x>t]
#Saat menguji model seperti ini, lebih baik pakai penaksiran sederhana seperti metode momen supaya tidak mengambil waktu di algoritma
#untuk cari dist. terbaik, gunakan metode penaksiran sederhana
#setelah dapat model2 yg baik, gunakan metode penaksiran yg lebih sophisticated: bayesian,MLE
#Kalau misal model pertama adalah eksponensial, taksiran teta kita sama dengan x bar, jadi cari mean(xt)
#hasil teta mirip dengan mean dist. gamma dengan parameter gamma(3,7)
teta = mean(xt)



# CALON PERTAMA KITA PAKAI DIST. GAMMA
#huruf p di pgamma maksudnya CDF gamma
#Kalau eksponensial,shape/ alphanya=1
#scale adalah teta distribusi eksponenisal
#H adalah titik pemancungan
#verticals ditulis karena memang syntax
dplot(xt,"pgamma",list(shape=1,scale=teta),H=5,vertical=TRUE)
#kelihatan bahwa utk 97 data pun, yg biru(model)(CDF truncated eksponensial) masih jauh dari empirical dist. function kita
#kata pak rizky, eksponensial dibuang aja karena terlalu jauh



#CALON KE-2 KITA PAKAI GAMMA
#Kalau pakai metode momen, penaksiran alpha = (xbar^2)/(s^2)
a=(mean(xt)^2)/var(xt)
#beta adalah (s^2)/(xbar)
b=var(xt)/mean(xt)
#bisa dilihat di environment hasil estimasi MoM a dan b
#a = 4.417
#b = 4.791
#Jadi, agak jauh tapi gpp
#Kita lakukan pgamma lagi, tapi shape=a dan scale=b
dplot(xt,"pgamma",list(shape=a,scale=b),H=5,vertical=TRUE)
#Kita lihat sekalipun taksiran parameternya jauh dari metode MoM sehingga taksirannya meleset,
#Kita lihat disini Fn (Empirical dist. function) cukup mirip dengan grafik truncated gamma dist. kita
# Jadi, untuk tahap ini kita bisa keep model gamma ini lumayan cocok sebagai calon
#Next, kalau punya weibull lakukan hal yg sama
#Jadi semua dist yg di list bisa dilist dan liat mana yang dibuang dan yg tidak sehingga punya calon lebih sedikit
#Kalau di dplot pakai "pnormal", listnya diisi mean dan sd


# BUAT QQ PLOT UNTUK DATA xt DENGAN DISTRIBUSI EXP(TETA)
#Sekarang coba kita lakukan pada dist. eksponensial
#Karena dist. eksponensial, maka shape kita ubah menjadi 1
#Sedangkan parameter scale dipakai teta karena exp(teta)
#vertical=TRUE tidak perlu. Mungkin karena pakai qplot
qplot(xt,"pgamma",list(shape=1,scale=teta),H=5)
#bisa dilihat setelah 20, hasil qqplot jelek karena semakin menjauhi garis y=x
#garis lurus adalah y=x

# BUAT QQ PLOT UNTUK DATA xt DENGAN DISTRIBUSI GAMMA(a,b)
qplot(xt,"pgamma",list(shape=a,scale=b),H=5)
#qq plot yang ini lumayan
#mungkin bagian kanan bisa dibilang tidak bagus
#tapi ingat ini data ekstrim, jadi bisa dianggap ada anomali gpp
#Karena namanya data yang ttnya tinggi(?) excess yang tinggi biasa mulai jarang datang kecuali ekor tebal
#Jadi, suka terjadi perubahan pola di ujung sana
#Jadi, secara umum kita bilang dia cukup mendekati garis y=x
#sehingga model gamma bisa dikeep

# KOLOMOGOROV SMIRNOV TEST UNTUK DIST EXP(TETA)
#fungsi ks.test ini sebenernya untuk data lengkap, cuma kalo pakai truncgof, bisa dipakai buat truncated
ks.test(xt,"pgamma",list(shape=1,scale=teta),H=5)
#didapat p - value nya 0.01
#Kalau pakai alpha = 0.05, maka H0 ditolak
#Jadi, dist. eksponensial ditolak


# KOLMOGOROV SMIRNOV TEST UNTUK DIST. GAMMA(a,b)
ks.test(xt,"pgamma",list(shape=a,scale=b),H=5)
#didapat p-value = 0.92 sangat bagus
#Jauh banget dari 0.05, jadi bisa dibilang H0 tidak ditolak
#Jadi, dist gamma sangat baik untuk menggambarkan data kita



# ANDERSON DARLING TEST UNTUK DIST. EKSPONENSIAL
ad.test(xt,"pgamma",list(shape=1,scale=teta),H=5)
#didapatkan p-value = 0.11
#kalau significant value = 0.05
#sebenarnya untuk menggunakan ad -test, H0 tidak ditolak
#hampir ditolak karena jarak 0.11 dekat ke 0.05, tapi tidak ditolak
#ini akan terjadi karena uji statistik tidak mutlak
#ingat uji statistik dengan significance level 0.05
#artinya ada kesalahan di sana, kesalahan jenis 2
#menerima H0 yang salah
#kalau kesalahan jenis 1 menolak H0 yang bener
#mungkin dikarenakan meng emphasize bagian tail nya

#ANDERSON DARLING TEST UNTUK DIST. GAMMA(a,b)
ad.test(xt,"pgamma",list(shape=a,scale=b),H=5)
#p - value nya sangat2 kecil
#justru dist. gamma ditolak di sini
#mungkin ada salah syntax? algoritma?
#bapaknya juga tidak tahu kenapa ditolak...

# CRAMER-VON MISES TEST UNTUK DIST EXP(TETA)
w2.test(xt,"pgamma",list(shape=1,scale=teta),H=5)
#dapat p -value =0.03
#sehingga H0 ditolak
#berarti sesuai karena data kita memang bukan eksponensial


# CRAMER-VON MISES TEST UNTUK DIST GAMMA(a,b)
w2.test(xt,"pgamma",list(shape=a,scale=b),H=5)
#p -value =0.82
#jadi H0 tidak ditolak dan p-valuenya bagus
#paling oke memang K-S test di sini
