Self Assestment R Skillacademy
================
Agung Surya Nugraha
4/25/2021

### Pendahuluan

Markdown yang dibuat menggunakan R berisi soal serta jawaban dengan
sedikit pembahasan dari program pelatihan PRAKERJA yaitu “Dasar
Programing untuk Data Science”. Data yang digunakan sebagai bahan dalam
mengerjakan tugas berasal dari skillacademy dengan nama
dataset\_superstore\_simple.csv dan ditampilkan beberapa value serta
variable yang dapat dilihat seperti berikut.

Superstore table.

| order\_id      | order\_date | customer\_id | segment   | category        | sub\_category |    sales | quantity |    profit |
|:---------------|:------------|:-------------|:----------|:----------------|:--------------|---------:|---------:|----------:|
| CA-2016-152156 | 2016-11-08  | CG-12520     | Consumer  | Furniture       | Bookcases     | 261.9600 |        2 |   41.9136 |
| CA-2016-152156 | 2016-11-08  | CG-12520     | Consumer  | Furniture       | Chairs        | 731.9400 |        3 |  219.5820 |
| CA-2016-138688 | 2016-06-12  | DV-13045     | Corporate | Office Supplies | Labels        |  14.6200 |        2 |    6.8714 |
| US-2015-108966 | 2015-10-11  | SO-20335     | Consumer  | Furniture       | Tables        | 957.5775 |        5 | -383.0310 |
| US-2015-108966 | 2015-10-11  | SO-20335     | Consumer  | Office Supplies | Storage       |  22.3680 |        2 |    2.5164 |


<br/> 1. Mencari **customer ID** yang memiliki **sales** paling besar.

``` r
superstore_data %>%
  filter(sales == max(sales))%>%
  select(customer_id,sales)
```

    ##   customer_id   sales
    ## 1    AB-10105 9892.74

<br/> 2. Mencari **sub-category** beserta total **profit** berdasarkan
**category** “Office Suplies”.

``` r
superstore_data %>% 
  group_by(sub_category) %>%
  filter(category == 'Office Supplies') %>%
  summarise(total_profit = sum(profit))
```

    ## # A tibble: 9 x 2
    ##   sub_category total_profit
    ##   <chr>               <dbl>
    ## 1 Appliances         18138.
    ## 2 Art                 6528.
    ## 3 Binders            30222.
    ## 4 Envelopes           6964.
    ## 5 Fasteners            950.
    ## 6 Labels              5546.
    ## 7 Paper              34054.
    ## 8 Storage            21279.
    ## 9 Supplies           -1189.

<br/> 3. Berapa banyak **order** yang menghasilkan **profit**
negatif(rugi).

``` r
order_with_negatif_profit <- superstore_data %>%
  filter(profit < 0)

count(order_with_negatif_profit)
```

    ##      n
    ## 1 1869

<br/> 4. Mencari total sales terbanyak antara 3 customer\_id yaitu:
JE-16165, KH-16510, AD-10180.

``` r
superstore_data %>%
  group_by(customer_id) %>%
  filter(customer_id =='JE-16165' | customer_id == 'KH-16510' | customer_id == 'AD-10180') %>%
  summarise(total_sales = sum(sales)) %>%
  arrange(desc(total_sales))
```

    ## # A tibble: 3 x 2
    ##   customer_id total_sales
    ##   <chr>             <dbl>
    ## 1 AD-10180          6107.
    ## 2 KH-16510          5953.
    ## 3 JE-16165          2697.

<br/> 5. Membuat data frame baru berisi total sales, jumlah customsr dan
total profit tiap tahun.

``` r
superstore_data$order_date <- as.Date(paste(superstore_data$order_date,sep=""))

superstore_data %>%
  mutate(year = format(order_date, format="%Y")) %>%
  group_by(year) %>%
  summarise(total_customer = n_distinct(customer_id), total_sales = sum(sales))
```

    ## # A tibble: 4 x 3
    ##   year  total_customer total_sales
    ##   <chr>          <int>       <dbl>
    ## 1 2014             595     461609.
    ## 2 2015             573     470533.
    ## 3 2016             638     587206.
    ## 4 2017             693     697515.

<br/> 6. Buatlah scatterplot antara sales dan profit untuk data di tahun
2014-2015 saja, bedakan warnanya antara tahun 2014 dan tahun 2015.

![](present_files/figure-gfm/soal%206%20plot-1.png)<!-- --> <br/><br/>
7. Buatlah barchart yang berisi total profit dari 10 customer dengan
total sales tertinggi di tahun 2015.

![](present_files/figure-gfm/soal%207%20plot-1.png)<!-- --> <br><br>

### Summary

Dengan mengerjakan beberapa soal tersebut, dimaksudkan untuk lebih
mengenal dasar-dasar pengunaan library dan beberapa fungsi yang
digunakan dalam melakukan manipulasi data serta pembuatan visualisasi
sederhana.
