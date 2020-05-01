*&---------------------------------------------------------------------*
*& Report  ZTESTABAPGIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZTESTABAPGIT.


FUNCTION ZFM_IGS_API_GIT.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(VENDOR_NUM) TYPE  LIFNR
*"     VALUE(LOT_NUMBER) TYPE  LICHN OPTIONAL
*"     REFERENCE(WAFER_NUM) TYPE  CHAR2
*"  EXPORTING
*"     REFERENCE(RESPONSE) TYPE  ZIGS_RESP_TABLE
*"     REFERENCE(EV_PASS_QTY) TYPE  KWMENG
*"     REFERENCE(EV_FAIL_QTY) TYPE  KWMENG
*"--------------------------------------------------------------------

  DATA: lo_http_client     TYPE REF TO if_http_client,
        lo_streamfactory   TYPE REF TO if_ixml_stream_factory,
        lo_istream         TYPE REF TO if_ixml_istream,
        lo_document        TYPE REF TO if_ixml_document,
        lo_parser          TYPE REF TO if_ixml_parser,
        lo_weather_element TYPE REF TO if_ixml_element,
        lo_weather_nodes   TYPE REF TO if_ixml_node_list,
        lo_curr_node       TYPE REF TO if_ixml_node,
        lo_ixml            TYPE REF TO if_ixml.


  DATA: lv_service     TYPE string VALUE 'http://xmlrpc.maxim-ic.com/rpc',
*        lv_response    TYPE string,
        lv_response    TYPE xstring,
        lv_value       TYPE string,
        lv_node_length TYPE i,
        lv_node_index  TYPE i,
        lv_request     TYPE xstring,
        lv_node_name   TYPE string,
        lv_node_value  TYPE string.
*        lv_hier        TYPE int1.


  TYPES: BEGIN OF ty_count ,
           count TYPE i,
           index TYPE sy-tabix,
           lot   TYPE char255,
         END OF ty_count.

  FIELD-SYMBOLS  : <fs_name>  TYPE table .

  DATA : lt_final_lot     TYPE STANDARD TABLE OF smum_xmltb,
         lt_final_lot_tmp TYPE STANDARD TABLE OF smum_xmltb,
         ls_final_lot     TYPE smum_xmltb,
         lt_lot_det       TYPE STANDARD TABLE OF ty_count,
         lv_fg            TYPE c,
         lv_f_index       TYPE sy-tabix,
         lv_n_index       TYPE sy-tabix,
         lv_t_index       TYPE sy-tabix,
         lv_lines         TYPE i,
         lwa_lot_det      TYPE ty_count,
         lv_tot_pass      TYPE kwmeng,
         lv_tot_fail      TYPE kwmeng.

  DATA: lc_step TYPE string VALUE 'STEPPER,INS_BUMP,PST_BUMP,QC,PST_BUMP_PAT,INS_SAW'.

  CALL METHOD cl_http_client=>create_by_url(
    EXPORTING
      url                = lv_service
    IMPORTING
      client             = lo_http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4 ).

  .

*  CONCATENATE '<?xml version="1.0"?>'
*'<methodCall>'
*   '<methodName>maxvision.get_cache_composite_counts_by_parent_lot</methodName>'
*      '<params>'
*      '<param>'
*         '<value><string>' vendor_num '</string></value>'
*         '</param>'
*      '</params>'
*  '</methodCall>' INTO lv_request.
  DATA : lv_request1 TYPE string.
  DATA : lv_request2 TYPE string.
  DATA : lv_request11 TYPE xstring.
  DATA : lv_request22 TYPE xstring.
  DATA: lv_x TYPE char1024.

  CONCATENATE '<?xml version="1.0"?>'
'<methodCall>'
'<methodName>maxvision.get_cached_map_hash</methodName>'
'<params>'
  '<param>'
        '<value><string>' vendor_num '</string></value>'
   '</param>'
   '<param>'
     '<value><string>' wafer_num '</string></value>'
   '</param>'
   '<param>'
   INTO lv_request1.
*
*lv_request = |<?xml version="1.0"?><methodCall><methodName>maxvision.get_cached_map_hash</methodName>|
*               && |<params><param><value><string>| && vendor_num && |</string></value></param>| &&
*               |<param><value><string>| && wafer_num && | </string></value>| &&
*               |</param><param><value>| && lc_step && |</value></param>| &&
*               |<param><value><string>| && 1 && |</string></value></param>| &&
*               |<param><value></value></param>| &&
*               |</params></methodCall>| .

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = lv_request1
    IMPORTING
      buffer = lv_request11
    EXCEPTIONS
      failed = 1
      OTHERS = 2.

  CONCATENATE '<value>' lc_step '</value>'
      '</param>'
    '<param>'
       '<value><string>1</string></value>'
    '</param>'
    '<param>'
          '<value></value>'
    '</param>'
    '</params>'
   '</methodCall> ' INTO lv_request2.


  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = lv_request2
    IMPORTING
      buffer = lv_request22
    EXCEPTIONS
      failed = 1
      OTHERS = 2.

  CONCATENATE lv_request11 lv_request22 INTO lv_request IN BYTE MODE.


*Set REQUEST METHOD
  CALL METHOD lo_http_client->request->set_header_field
    EXPORTING
      name  = 'User-Agent'
      value = 'Frontier/5.1.2'.

  CALL METHOD lo_http_client->request->set_header_field
    EXPORTING
      name  = 'Host'
      value = 'betty.userland.com'.

  CALL METHOD lo_http_client->request->set_header_field
    EXPORTING
      name  = 'Content-Type'
      value = 'text/xml'.

  CALL METHOD lo_http_client->request->set_data
    EXPORTING
      data   = lv_request
      offset = 0.
*      length = xml_length.


  lo_http_client->send(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2 ).

  lo_http_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3 ).

  CLEAR lv_response .
*  lv_response = lo_http_client->response->get_cdata( ).
  lv_response = lo_http_client->response->get_data( ).

  lo_ixml = cl_ixml=>create( ).
  lo_streamfactory = lo_ixml->create_stream_factory( ).
  lo_istream = lo_streamfactory->create_istream_xstring(
                                   lv_response ).
  lo_document = lo_ixml->create_document( ).
  lo_parser = lo_ixml->create_parser(
                         stream_factory = lo_streamfactory
                         istream        = lo_istream
                         document       = lo_document ).

  lo_parser->parse( ).

  DATA:   g_t_xml_info TYPE TABLE OF smum_xmltb.
  DATA :  g_t_return TYPE TABLE OF bapiret2.
  CALL FUNCTION 'SMUM_XML_PARSE'
    EXPORTING
      xml_input = lv_response
    TABLES
      xml_table = g_t_xml_info
      return    = g_t_return
    EXCEPTIONS
      OTHERS    = 0.
  IF sy-subrc EQ 0.

    READ TABLE g_t_xml_info ASSIGNING FIELD-SYMBOL(<fs_xml>) WITH KEY cvalue = 'PASS'.
    IF sy-subrc EQ 0.
      DATA(lv_index) = sy-tabix.
      DATA(lv_hier) = <fs_xml>-hier + 1.
      LOOP AT g_t_xml_info ASSIGNING <fs_xml> FROM lv_index WHERE hier = lv_hier AND type = 'V' AND cname = 'int' .
        ev_pass_qty = <fs_xml>-cvalue.
        EXIT.
      ENDLOOP.

    ENDIF.
    READ TABLE g_t_xml_info ASSIGNING <fs_xml> WITH KEY cvalue = 'FAIL'.
    IF sy-subrc EQ 0.
      lv_index = sy-tabix.
      lv_hier = <fs_xml>-hier + 1.
      LOOP AT g_t_xml_info ASSIGNING <fs_xml> FROM lv_index WHERE hier = lv_hier AND type = 'V' AND cname = 'int' .
        ev_fail_qty = <fs_xml>-cvalue.
        EXIT.
      ENDLOOP.

    ENDIF.

  ENDIF.


ENDFUNCTION.
