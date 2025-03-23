interface ZIF_C8A017_HTML_HANDLER
  public .


  methods ON_SAPEVENT_IN_HTML
    for event SAPEVENT of CL_ABAP_BROWSER
    importing
      !ACTION
      !QUERY_TABLE .
  methods ON_CLOSED_WINDOW
    for event CLOSED of CL_ABAP_BROWSER .
endinterface.
