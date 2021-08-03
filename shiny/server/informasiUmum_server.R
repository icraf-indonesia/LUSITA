# Section informasi umum pamTemplate---------------------------------------------
observe({
  updateSelectInput(
    session,
    "kom",
    choices = komoditas %>%
      filter(sut == input$sut) %>%
      select(nama_komoditas) %>%
      .[[1]]
  )
})

observe({
  updateSelectInput(
    session,
    "selected_provinsi",
    choices = komoditas %>%
      filter(nama_komoditas == input$kom) %>%
      select(provinsi) %>%
      .[[1]]
  )
})

observe({
  updateSelectInput(
    session,
    "th",
    choices = komoditas %>%
      filter(provinsi == input$selected_provinsi) %>%
      select(tahun_analisis) %>%
      .[[1]]
  )
})

observe({
  updateSelectInput(
    session,
    "tipeLahan",
    choices = komoditas %>%
      filter(tahun_analisis == input$th) %>%
      select(tipe_lahan) %>%
      .[[1]]
  )
})

# End - Section informasi umum pamTemplate---------------------------------------------

# Section informasi umum pamParsial---------------------------------------------
observe({
  updateSelectInput(
    session,
    "kom_par",
    choices = komoditas %>%
      filter(sut == input$sut_par) %>%
      select(nama_komoditas) %>%
      .[[1]]
  )
})

observe({
  updateSelectInput(
    session,
    "selected_provinsi_par",
    choices = komoditas %>%
      filter(nama_komoditas == input$kom_par) %>%
      select(provinsi) %>%
      .[[1]]
  )
})

observe({
  updateSelectInput(
    session,
    "th_par",
    choices = komoditas %>%
      filter(provinsi == input$selected_provinsi_par) %>%
      select(tahun_analisis) %>%
      .[[1]]
  )
})

observe({
  updateSelectInput(
    session,
    "tipeLahan_par",
    choices = komoditas %>%
      filter(tahun_analisis == input$th_par) %>%
      select(tipe_lahan) %>%
      .[[1]]
  )
})

# End - Section informasi umum pamParsial---------------------------------------------
