class ZCL_ZBSR_DASHBOARD_DPC_EX definition
  public
  inheriting from ZCL_ZBSR_DASHBOARD_DPC
  create public .

public section.

  types:
    begin of TS_BUCP_PARAM,
     REQ_TYPE type C length 160,
     REQ_TYPEID type C length 160,
  end of TS_BUCP_PARAM .
  types:
    TT_BUCP_PARAM type standard table of TS_BUCP_PARAM .
protected section.

  methods BSRAUTHSET_GET_ENTITY
    redefinition .
  methods BSRAUTHSET_GET_ENTITYSET
    redefinition .
  methods BSRCREATEDBYSET_GET_ENTITY
    redefinition .
  methods BSRCREATEDBYSET_GET_ENTITYSET
    redefinition .
  methods BSRDATASET_GET_ENTITY
    redefinition .
  methods BSRDATASET_GET_ENTITYSET
    redefinition .
  methods BSRDEFAULTSSET_GET_ENTITYSET
    redefinition .
  methods BSRSPLINSTRSET_GET_ENTITYSET
    redefinition .
  methods MATERIALSEARCHSE_GET_ENTITYSET
    redefinition .
  methods REQUESTTYPESET_GET_ENTITYSET
    redefinition .
  methods STATUSSET_GET_ENTITYSET
    redefinition .
  methods VENDORSEARCHSET_GET_ENTITYSET
    redefinition .
  methods WIPSET_GET_ENTITYSET
    redefinition .
  methods BSRSPECINSTRSET_GET_ENTITYSET
    redefinition .
private section.

  data LT_REQUEST .
ENDCLASS.



CLASS ZCL_ZBSR_DASHBOARD_DPC_EX IMPLEMENTATION.


  METHOD BSRAUTHSET_GET_ENTITY.
    DATA ls_auth TYPE  zcl_zbsr_form_mpc_ext=>ts_bsrauth.
    DATA lv_reqid TYPE c LENGTH 12.
    DATA lv_role TYPE c LENGTH 10.

    CHECK NOT it_key_tab[] IS INITIAL.
    READ TABLE it_key_tab INTO DATA(ls_key1) WITH KEY name = 'ReqId'.
    IF sy-subrc IS INITIAL.
      lv_reqid = ls_key1-value.
    ENDIF.
    CLEAR ls_key1.
    READ TABLE it_key_tab INTO ls_key1 WITH KEY name = 'Auth'.
    IF sy-subrc IS INITIAL.
      lv_role = ls_key1-value.
    ENDIF.
    SELECT SINGLE req_status FROM zbsr_header INTO @DATA(lv_status) WHERE req_id EQ @lv_reqid.
    IF sy-subrc IS INITIAL.
      CASE lv_status.
        WHEN '1'.
          lv_role = 'SHIP'.
        WHEN '2' OR '99'.
          lv_role = 'DISP'.
      ENDCASE.
    ENDIF.

    CHECK NOT lv_role IS INITIAL.
    AUTHORITY-CHECK OBJECT 'ZBSR_FORM' ID 'BSR_AUTH' FIELD lv_role.
    IF sy-subrc IS INITIAL.
      ls_auth-success = 'Y'.
    ELSE.
      ls_auth-success = 'N'.
    ENDIF.
    ls_auth-auth    = lv_role.
    ls_auth-req_id  = lv_reqid.
    er_entity = ls_auth.

  ENDMETHOD.


  METHOD BSRAUTHSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->BSRAUTHSET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    DATA lt_auth TYPE  zcl_zbsr_form_mpc_ext=>tt_bsrauth.
    DATA ls_auth TYPE  zcl_zbsr_form_mpc_ext=>ts_bsrauth.
    DATA lv_reqid TYPE zbsr_header-req_id.
    DATA lv_role TYPE c LENGTH 10.
    CHECK NOT it_key_tab[] IS INITIAL.
    READ TABLE it_key_tab INTO DATA(ls_key1) WITH KEY name = 'ReqId'.
    IF sy-subrc IS INITIAL.
      lv_reqid = ls_key1-value.
    ENDIF.
    CLEAR ls_key1.
    READ TABLE it_key_tab INTO ls_key1 WITH KEY name = 'Auth'.
    IF sy-subrc IS INITIAL.
      lv_role = ls_key1-value.
    ENDIF.
    IF NOT lv_reqid IS INITIAL.
      SELECT SINGLE req_status FROM zbsr_header INTO @DATA(lv_status) WHERE req_id EQ @lv_reqid.
      IF sy-subrc IS INITIAL.
        CASE lv_status.
          WHEN '1'.
            lv_role = 'SHIP'.
          WHEN '2' OR '99'.
            lv_role = 'DISP'.
        ENDCASE.
      ENDIF.
    ENDIF.

    CHECK NOT lv_role IS INITIAL.
    AUTHORITY-CHECK OBJECT 'ZBSR_FORM' ID 'BSR_AUTH' FIELD lv_role.
    IF sy-subrc IS INITIAL.
      ls_auth-success = 'Y'.
    ELSE.
      ls_auth-success = 'N'.
    ENDIF.
    ls_auth-auth    = lv_role.
    ls_auth-req_id  = lv_reqid.
    APPEND ls_auth TO lt_auth.
    CLEAR ls_auth.
    et_entityset = lt_auth.
  ENDMETHOD.


  METHOD BSRCREATEDBYSET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->BSRCREATEDBYSET_GET_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_request_object       =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
**  IMPORTING
**    er_entity               =
**    es_response_context     =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    DATA ls_createdby TYPE zcl_zbsr_dashboard_mpc_ext=>ts_bsrcreatedby.
    er_entity = ls_createdby.
  ENDMETHOD.


  METHOD BSRCREATEDBYSET_GET_ENTITYSET.
    DATA ra_emp TYPE RANGE OF zempid.
    DATA lt_createdby TYPE zcl_zbsr_dashboard_mpc_ext=>tt_bsrcreatedby.
    SELECT created_by FROM zbsr_header INTO TABLE @DATA(lt_bsrheader) WHERE created_by IS NOT NULL.
    IF sy-subrc IS INITIAL.
      SELECT bname, accnt FROM usr02 INTO TABLE @DATA(lt_usr02) FOR ALL ENTRIES IN @lt_bsrheader WHERE bname EQ @lt_bsrheader-created_by.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_usr02 ASSIGNING FIELD-SYMBOL(<fs>).
          APPEND INITIAL LINE TO ra_emp ASSIGNING FIELD-SYMBOL(<fs_emp>).
          <fs_emp>-sign   = 'I'.
          <fs_emp>-option = 'EQ'.
          <fs_emp>-low    = <fs>-accnt.
        ENDLOOP.
        SELECT employee_id, sapusername , displayname FROM zworkday_data INTO TABLE @DATA(lt_workday) WHERE employee_id IN @ra_emp.
      ENDIF.
      LOOP AT lt_workday INTO DATA(ls_workday).
        READ TABLE lt_usr02 INTO DATA(ls_usr02) WITH KEY accnt = ls_workday-employee_id.
        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entity>).
        <fs_entity>-created = ls_usr02-bname.
        <fs_entity>-created_by = ls_workday-displayname.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  method BSRDATASET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->BSRDATASET_GET_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_request_object       =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
**  IMPORTING
**    er_entity               =
**    es_response_context     =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    select SINGLE * FROM zbsr_header INTO CORRESPONDING FIELDS OF ER_ENTITY.
  endmethod.


  METHOD BSRDATASET_GET_ENTITYSET.
    DATA:

      lt_req_id       TYPE TABLE OF          selopt,
      lt_wip          TYPE TABLE OF          selopt,
      lt_mfpnr        TYPE TABLE OF          selopt,
      lt_matnr_temp   TYPE TABLE OF          selopt,
      lt_status       TYPE TABLE OF          selopt,
      lt_reqtype      TYPE TABLE OF          selopt,
      lt_create_by    TYPE TABLE OF         selopt,
      lt_create_on    TYPE  TABLE OF       selopt,
      lt_vbatch       TYPE TABLE OF           selopt,
      lt_specificinst TYPE TABLE OF          selopt,
      lt_specialinstr TYPE TABLE OF          selopt.

    DATA: ls_so           TYPE                   /iwbep/s_cod_select_option,
          ls_req_id       TYPE                   selopt,
          ls_wip          TYPE                   selopt,
          ls_mfpnr        TYPE                   selopt,
          ls_matnr_temp   TYPE                   selopt,
          ls_status       TYPE                   selopt,
          ls_reqtype      TYPE                   selopt,
          ls_create_by    TYPE                   selopt,
          ls_create_on    TYPE                   selopt,
          ls_vbatch       TYPE                   selopt,
          ls_specificinst TYPE           selopt,
          ls_specialinstr TYPE          selopt,
          ls_bucp         TYPE zcl_zbsr_dashboard_mpc=>ts_requesttype,
          lt_bucp_data    TYPE zcl_zbsr_dashboard_mpc=>tt_requesttype.

    IF  it_filter_select_options IS NOT INITIAL.
      LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options).
        CASE ls_filter_select_options-property .
          WHEN 'ReqId'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_req_id.
              INSERT ls_req_id INTO TABLE lt_req_id.
              CLEAR ls_req_id.
            ENDLOOP.

          WHEN 'WipLocation'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO   ls_wip  .
              INSERT   ls_wip  INTO TABLE   lt_wip  .
              CLEAR ls_wip.
            ENDLOOP.
            DELETE lt_wip WHERE low IS INITIAL.
          WHEN 'Mfrpn'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO  ls_mfpnr  .
              TRANSLATE ls_mfpnr-low TO UPPER CASE.
              INSERT  ls_mfpnr  INTO TABLE   lt_mfpnr .
              CLEAR ls_mfpnr.
            ENDLOOP.
            IF lt_mfpnr IS NOT INITIAL.
              SELECT matnr FROM mara INTO TABLE @DATA(lt_mara) WHERE mfrpn IN @lt_mfpnr .
              IF sy-subrc EQ 0.
                ls_matnr_temp-sign = 'I'.
                ls_matnr_temp-option = 'EQ'.
                LOOP AT lt_mara ASSIGNING FIELD-SYMBOL(<ls_mara>).
                  ls_matnr_temp-low = <ls_mara>-matnr.
                  APPEND ls_matnr_temp TO lt_matnr_temp.
                  CLEAR ls_matnr_temp-low.
                ENDLOOP.
              ENDIF.
            ENDIF.
          WHEN 'SpecialInstr'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO  ls_specialinstr.
              INSERT   ls_specialinstr INTO TABLE  lt_specialinstr .
              CLEAR ls_specialinstr.
            ENDLOOP.
          WHEN 'SpecificInst'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO  ls_specificinst  .
              INSERT  ls_specificinst  INTO TABLE  lt_specificinst .
              CLEAR ls_specificinst.
            ENDLOOP.
          WHEN 'Status'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO  ls_status  .
              INSERT   ls_status  INTO TABLE  lt_status .
              CLEAR ls_status.
            ENDLOOP.
            DELETE lt_status WHERE low IS INITIAL.
          WHEN 'ReqType'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO  ls_reqtype  .
              INSERT   ls_reqtype  INTO TABLE  lt_reqtype .
              CLEAR ls_reqtype.
            ENDLOOP.
            DELETE lt_reqtype WHERE low IS INITIAL.
          WHEN 'CreatedBy'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO  ls_create_by  .
              TRANSLATE ls_create_by-low TO UPPER CASE.
              INSERT   ls_create_by  INTO TABLE  lt_create_by .
              CLEAR ls_create_by.
            ENDLOOP.
          WHEN 'CreatedOn'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO  ls_create_on  .
              INSERT   ls_create_on  INTO TABLE  lt_create_on .
              CLEAR ls_create_on.
            ENDLOOP.
          WHEN 'VendorBatch'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO  ls_vbatch .
              INSERT   ls_vbatch  INTO TABLE  lt_vbatch .
              CLEAR ls_vbatch.
            ENDLOOP.
            IF lt_vbatch IS NOT INITIAL.
              SELECT req_id FROM zbsr_item INTO TABLE @DATA(lt_item) WHERE vendor_batch IN @lt_vbatch.
              IF sy-subrc EQ 0.
                ls_req_id-sign = 'I'.
                ls_req_id-option = 'EQ'.
                LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<ls_item>).
                  ls_req_id-low = <ls_item>-req_id.
                  APPEND ls_req_id TO lt_req_id.
                  CLEAR ls_req_id-low.
                ENDLOOP.
              ENDIF.
            ENDIF.
        ENDCASE.
      ENDLOOP.

      SELECT * FROM zbsr_header
        INTO TABLE @DATA(lt_header)
        WHERE req_id IN @lt_req_id
        AND req_type IN @lt_reqtype
        AND req_status IN @lt_status
        AND special_instr IN @lt_specialinstr
        AND wip_location IN @lt_wip
        AND sap_material IN @lt_matnr_temp
        AND specific_inst IN @lt_specificinst
        AND created_by IN @lt_create_by
        AND created_on IN @lt_create_on .
      IF  sy-subrc NE 0.

        SELECT * FROM zbsr_header
          INTO TABLE lt_header
          WHERE req_id IN lt_req_id
          AND req_type IN lt_reqtype
          AND req_status IN lt_status
          AND special_instr IN lt_specialinstr
          AND wip_location IN lt_wip
          AND output_material IN lt_matnr_temp
          AND specific_inst IN lt_specificinst
          AND created_by IN lt_create_by
          AND created_on IN lt_create_on .
      ENDIF.
    ELSE.
      SELECT * FROM zbsr_header INTO CORRESPONDING FIELDS OF TABLE et_entityset UP TO 20 ROWS ORDER BY created_on DESCENDING.
    ENDIF.


    DATA : ra_emp TYPE RANGE OF zempid.

    IF NOT lt_header IS INITIAL.
      SELECT bname, accnt FROM usr02 INTO TABLE @DATA(lt_usr02) FOR ALL ENTRIES IN @lt_header WHERE bname EQ @lt_header-created_by.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_usr02 ASSIGNING FIELD-SYMBOL(<fs_usr02>).
          APPEND INITIAL LINE TO ra_emp ASSIGNING FIELD-SYMBOL(<fs_emp>).
          <fs_emp>-sign    = 'I'.
          <fs_emp>-option  = 'EQ'.
          <fs_emp>-low     = <fs_usr02>-accnt.
        ENDLOOP.
        SELECT employee_id, sapusername , displayname FROM zworkday_data INTO TABLE @DATA(lt_workday) WHERE employee_id IN @ra_emp.
      ENDIF.
    ENDIF.

    SELECT name,field1,field2 FROM zbucp_config INTO TABLE @DATA(lt_bucp) WHERE name IN ( 'REQUEST_TYPE' , 'REQ_STATUS', 'WIP_LOCATION' ,'DEPARTMENT', 'SPECIAL_INSTRUCTION', 'SPECIFIC_INSTRUCTION'  )
                                                                            AND field5 = 'BSR'.
    IF sy-subrc EQ 0.
      LOOP AT lt_header INTO DATA(ls_header).
        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs1>).
        MOVE-CORRESPONDING ls_header TO <fs1>.
*        <fs1>-dats = '20180101'.
        READ TABLE lt_bucp INTO DATA(wa_bucp) WITH KEY name = 'REQ_STATUS' field1 = ls_header-req_status .
        IF sy-subrc EQ 0.
          <fs1>-req_status = wa_bucp-field2.
          CLEAR wa_bucp.
        ENDIF.

        READ TABLE lt_bucp INTO wa_bucp WITH KEY name = 'DEPARTMENT' field1 = ls_header-department .
        IF sy-subrc EQ 0.
          <fs1>-department = wa_bucp-field2.
          CLEAR wa_bucp.
        ENDIF.

        READ TABLE lt_bucp INTO wa_bucp WITH KEY name = 'REQUEST_TYPE' field1 = ls_header-req_type.
        IF sy-subrc EQ 0.
          <fs1>-req_type = wa_bucp-field2.
          CLEAR wa_bucp.
        ENDIF.

        READ TABLE lt_bucp INTO wa_bucp WITH KEY name = 'WIP_LOCATION' field1 = ls_header-wip_location .
        IF sy-subrc EQ 0.
          <fs1>-wip_location  = wa_bucp-field2.
          CLEAR wa_bucp.
        ENDIF.

        READ TABLE lt_bucp INTO wa_bucp WITH KEY name = 'SPECIAL_INSTRUCTION' field1 = ls_header-special_instr .
        IF sy-subrc EQ 0.
          <fs1>-special_instr = wa_bucp-field2.
          CLEAR wa_bucp.
        ENDIF.

        READ TABLE lt_bucp INTO wa_bucp WITH KEY name = 'SPECIFIC_INSTRUCTION' field1 = ls_header-specific_inst .
        IF sy-subrc EQ 0.
          <fs1>-specific_inst = wa_bucp-field2.
          CLEAR wa_bucp.
        ENDIF.



        IF NOT ls_header-sap_material IS INITIAL .
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
            EXPORTING
              input  = ls_header-sap_material
            IMPORTING
              output = <fs1>-mfrpn.
        ENDIF.
        IF NOT ls_header-created_by IS INITIAL.
          CLEAR <fs1>-created_by.
          READ TABLE lt_usr02 ASSIGNING <fs_usr02> WITH KEY bname = ls_header-created_by.
          IF sy-subrc IS INITIAL.
            READ TABLE lt_workday INTO DATA(ls_workday) WITH KEY employee_id = <fs_usr02>-accnt.
            IF sy-subrc IS INITIAL.
              <fs1>-created_by = ls_workday-displayname.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    SORT et_entityset BY created_on DESCENDING req_id DESCENDING .
  ENDMETHOD.


  METHOD BSRDEFAULTSSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->BSRDEFAULTSSET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

*    DATA : ls_entity TYPE zcl_zbsr_dashboard_mpc=>ts_bsrdefaults.
**
***    LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options).
***      CASE ls_filter_select_options-property .
***        WHEN 'REQUEST_TYPE'.
***
*** SELECT field1,field2 FROM zbucp_config INTO TABLE @DATA(lt_bucp) WHERE name = 'REQUEST_TYPE' and FIELD5 = 'BSR'.
***          IF sy-subrc EQ 0.
***            LOOP AT lt_bucp ASSIGNING FIELD-SYMBOL(<fs>).
***              ls_entity-req_type = <fs>-field2.
***              APPEND ls_entity TO et_entityset.
***            ENDLOOP.
***          ENDIF.
***      ENDCASE.
***    ENDLOOP.
*
*    DATA : lt_entity TYPE TABLE OF zcl_zbsr_dashboard_mpc=>ts_bsrdefaults.
*
*    SELECT field1,field2 FROM zbucp_config INTO TABLE @DATA(lt_bucp) WHERE name = 'REQUEST_TYPE'." AND field5 = 'BSR'.
*    IF sy-subrc EQ 0.
*      LOOP AT lt_bucp ASSIGNING FIELD-SYMBOL(<fs>).
*        ls_entity-req_index = sy-tabix.
*        ls_entity-req_typeid = <fs>-field1.
*        ls_entity-req_type = <fs>-field2.
*        APPEND ls_entity TO et_entityset.
*        CLEAR ls_entity.
*      ENDLOOP.
*    ENDIF.
*
*
*    SELECT ddtext,domvalue_l
*        FROM dd07t INTO TABLE @DATA(lt_status_desc)
*        WHERE domname = 'ZREQ_STATUS_DO'
*        AND ddlanguage = @sy-langu
*        AND as4local = 'A'.
*    IF sy-subrc EQ 0.
*      LOOP AT lt_status_desc ASSIGNING FIELD-SYMBOL(<fs_status>).
*        ls_entity-req_index = sy-tabix.
*        ls_entity-req_statusid = <fs_status>-domvalue_l.
*        ls_entity-req_status = <fs_status>-ddtext.
**        APPEND ls_entity TO et_entityset.
*        MODIFY et_entityset INDEX sy-tabix FROM ls_entity TRANSPORTING req_index req_statusid req_status.
*        IF sy-subrc <> 0.
*          APPEND ls_entity TO et_entityset.
*        ENDIF.
*        CLEAR ls_entity.
*      ENDLOOP.
*    ENDIF.

*    Delete et_entityset WHERE Req_Type = ''.


*    DATA: lt_tab TYPE TABLE OF zbsr_s_defaults.
*    SELECT * FROM zbucp_config INTO CORRESPONDING FIELDS OF TABLE et_entityset.
  ENDMETHOD.


  METHOD BSRSPECINSTRSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->BSRSPECINSTRSET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    DATA lt_default_sp TYPE TABLE OF zcl_zbsr_dashboard_mpc=>ts_bsrspecinstr.
    SELECT * FROM zbucp_config INTO TABLE @DATA(lt_config) WHERE name EQ 'SPECIFIC_INSTRUCTION' AND type = 'DROP_DOWN' AND field5 EQ 'BSR'.
    CHECK sy-subrc IS INITIAL.
*    SELECT * FROM zbucp_config INTO TABLE @DATA(lt_config_sp) WHERE name EQ 'SPECIAL_INSTRUCTION' AND type   = 'DROP_DOWN'
*                                                                                                  AND field5 EQ 'BSR'.

    LOOP AT lt_config ASSIGNING FIELD-SYMBOL(<fs1>)." WHERE field4 = ls_config_sp-field2.
      APPEND INITIAL LINE TO lt_default_sp ASSIGNING FIELD-SYMBOL(<fs_sp>).
      <fs_sp>-specific_inst = <fs1>-field1.
      <fs_sp>-specific_inst_txt = <fs1>-field2.
      <fs_sp>-special_instr = <fs1>-field1.
    ENDLOOP.
    et_entityset = lt_default_sp.
  ENDMETHOD.


  method BSRSPLINSTRSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->BSRSPLINSTRSET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
     DATA lt_default_si TYPE TABLE OF zbsr_s_spec_instr_defaults.
        SELECT * FROM zbucp_config INTO TABLE @DATA(lt_config) WHERE name EQ 'SPECIAL_INSTRUCTION' AND type = 'DROP_DOWN' AND field5 EQ 'BSR'.
        CHECK sy-subrc IS INITIAL.
        LOOP AT lt_config ASSIGNING FIELD-SYMBOL(<fs1>).
          APPEND INITIAL LINE TO lt_default_si ASSIGNING FIELD-SYMBOL(<fs_si>).
          <fs_si>-special_instr     = <fs1>-field1.
          <fs_si>-special_instr_txt = <fs1>-field2.
        ENDLOOP.
        et_entityset = lt_default_si.
  endmethod.


  METHOD MATERIALSEARCHSE_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->MATERIALSEARCHSE_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
*    /IWBEP/T_COD_SELECT_OPTIONS
    DATA : ls_so    TYPE                   /iwbep/s_cod_select_option,
           ls_mfrpn TYPE                   selopt,
           lt_mfrpn TYPE TABLE OF          selopt.

    IF  it_filter_select_options[] IS NOT INITIAL.
      LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options).
        LOOP AT ls_filter_select_options-select_options INTO ls_so.
          MOVE-CORRESPONDING ls_so TO ls_mfrpn.
          APPEND ls_mfrpn TO lt_mfrpn.
        ENDLOOP.
      ENDLOOP.
       SELECT * FROM mara
         INTO CORRESPONDING FIELDS OF TABLE et_entityset
         WHERE mfrpn in lt_mfrpn.
   else.
      SELECT * FROM mara INTO CORRESPONDING FIELDS OF TABLE et_entityset  UP TO 10 ROWS.
    ENDIF.

  ENDMETHOD.


  METHOD REQUESTTYPESET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->REQUESTTYPESET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    DATA : ls_entity TYPE zcl_zbsr_dashboard_mpc=>ts_requesttype.

    SELECT field1,field2 FROM zbucp_config INTO TABLE @DATA(lt_bucp) WHERE name = 'REQUEST_TYPE'." AND field5 = 'BSR'.
    IF sy-subrc EQ 0.
      LOOP AT lt_bucp ASSIGNING FIELD-SYMBOL(<fs>).
        ls_entity-req_typeid = <fs>-field1.
        ls_entity-req_type = <fs>-field2.
        APPEND ls_entity TO et_entityset.
        CLEAR ls_entity.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD STATUSSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->STATUSSET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    DATA : ls_entity TYPE zcl_zbsr_dashboard_mpc=>ts_status.
*    SELECT ddtext,domvalue_l
*       FROM dd07t INTO TABLE @DATA(lt_status_desc)
*       WHERE domname = 'ZREQ_STATUS_DO'
*       AND ddlanguage = @sy-langu
*       AND as4local = 'A'.
    SELECT field1,field2 FROM zbucp_config INTO TABLE @DATA(lt_bucp) WHERE name = 'REQ_STATUS'.
    IF sy-subrc EQ 0.
      LOOP AT lt_bucp ASSIGNING FIELD-SYMBOL(<fs_status>).
        ls_entity-req_statusid =  <fs_status>-field1.
        ls_entity-req_status =  <fs_status>-field2.
        APPEND ls_entity TO et_entityset.
        CLEAR ls_entity.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD VENDORSEARCHSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->VENDORSEARCHSET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    DATA : ls_so    TYPE                   /iwbep/s_cod_select_option,
           ls_matnr TYPE                   selopt,
           lt_matnr TYPE TABLE OF          selopt,
           ls_licha TYPE                   selopt,
           lt_licha TYPE TABLE OF          selopt.

    IF  it_filter_select_options[] IS NOT INITIAL.
      LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options).
       CASE ls_filter_select_options-property .
        WHEN 'Matnr'.
        LOOP AT ls_filter_select_options-select_options INTO ls_so.
          MOVE-CORRESPONDING ls_so TO ls_matnr.
          CALL  FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input              = ls_matnr-low
         IMPORTING
           OUTPUT             = ls_matnr-low.
          APPEND ls_matnr TO lt_matnr.
          CLEAR ls_matnr.
        ENDLOOP.
        WHEN 'Licha'.
        LOOP AT ls_filter_select_options-select_options INTO ls_so.
          MOVE-CORRESPONDING ls_so TO ls_licha.
          APPEND ls_licha TO lt_licha.
          clear ls_licha.
        ENDLOOP.
       ENDCASE.
      ENDLOOP.

      SELECT matnr licha Vlwrk FROM mch1
        INTO CORRESPONDING FIELDS OF TABLE et_entityset
        WHERE matnr IN lt_matnr
        AND licha in lt_licha.
    ELSE.
      SELECT matnr licha Vlwrk FROM mch1 INTO CORRESPONDING FIELDS OF TABLE et_entityset  UP TO 10 ROWS.
    ENDIF.
  ENDMETHOD.


  METHOD WIPSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->WIPSET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    DATA : ls_entity TYPE zcl_zbsr_dashboard_mpc=>ts_wip.

    DATA : lt_entity TYPE TABLE OF zcl_zbsr_dashboard_mpc=>ts_wip.

    SELECT field1,field2 FROM zbucp_config INTO TABLE @DATA(lt_bucp) WHERE name = 'WIP_LOCATION' AND field5 = 'BSR'.
    IF sy-subrc EQ 0.
      LOOP AT lt_bucp ASSIGNING FIELD-SYMBOL(<fs>).
*        ls_entity-req_index = sy-tabix.
        ls_entity-wip_locationid = <fs>-field1.
        ls_entity-wip_location = <fs>-field2.
        APPEND ls_entity TO et_entityset.
        CLEAR ls_entity.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
