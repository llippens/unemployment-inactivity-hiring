metabin_cstm <-
  function(data){
    metabin(
      event.e = callback_count_non_working,
      n.e = application_count_non_working,
      event.c = callback_count_working,
      n.c = application_count_working,
      studlab = authors,
      data = data,
      cluster = cluster,
      method = "inverse",
      sm = "RR",
      MH.exact = TRUE,
      random = TRUE,
      method.tau = "REML",
      method.random.ci = "HK",
      #adhoc.hakn.ci = "se",
      method.predict = "HK",
      #adhoc.hakn.pi = "se",
      method.tau.ci = "QP"
    )
  }

metabin_cstm_uc <-
  function(data){
    metabin(
      event.e = callback_count_non_working,
      n.e = application_count_non_working,
      event.c = callback_count_working,
      n.c = application_count_working,
      studlab = authors,
      data = data,
      method = "inverse",
      sm = "RR",
      MH.exact = TRUE,
      random = TRUE,
      method.tau = "REML",
      method.random.ci = "HK",
      #adhoc.hakn.ci = "se",
      method.predict = "HK",
      #adhoc.hakn.pi = "se",
      method.tau.ci = "QP"
    )
  }
