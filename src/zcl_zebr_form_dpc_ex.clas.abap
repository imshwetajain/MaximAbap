class ZCL_ZEBR_FORM_DPC_EX definition
  public
  inheriting from ZCL_ZEBR_FORM_DPC
  create public .

public section.

  types:
    tt_zebr_t_mast_data  TYPE STANDARD TABLE OF zebr_t_mast_data .
  types:
    BEGIN OF  ty_mapl ,
        matnr TYPE matnr,
        werks TYPE werks_d,
        plnty TYPE mapl-plnty,
        plnnr TYPE mapl-plnnr,
        plnal TYPE mapl-plnal,
        zaehl type plko-zaehl,
        datuv type plko-datuv,
        statu type plko-statu,
        plnnr_alt   type plko-plnnr_alt,

      END OF  ty_mapl .
  types:
    BEGIN OF  ty_plas_plpo ,
        plnty TYPE  plas-plnty,
        plnnr TYPE  plas-plnnr,
        plnal TYPE  plas-plnal,
        plnkn TYPE  plas-plnkn,
        zaehl TYPE  plas-zaehl,
        datuv TYPE  plpo-datuv,
        loekz TYPE  plpo-loekz,
        vornr TYPE  plpo-vornr,
        werks TYPE  plpo-werks,
        lifnr TYPE  plpo-lifnr,
        infnr TYPE  plpo-infnr,
      END OF   ty_plas_plpo .
  types:
    BEGIN OF  ty_mch1 ,
        matnr TYPE matnr,
        charg TYPE charg_d,
        licha TYPE lichn,
        mtart TYPE mtart,
      END OF ty_mch1 .
  types:
    tt_mapl      TYPE STANDARD TABLE OF ty_mapl .
  types:
    tt_plas_plpo TYPE STANDARD TABLE OF ty_plas_plpo .
  types:
    tt_mch1      TYPE STANDARD TABLE OF  ty_mch1 .

  class-methods READ_TEXT
    importing
      !ID type THEAD-TDID
      !NAME type THEAD-TDNAME
      !OBJECT type THEAD-TDOBJECT
    returning
      value(TEXT) type STRING .
  class-methods SAVE_TEXT
    importing
      !ID type THEAD-TDID
      !NAME type THEAD-TDNAME
      !OBJECT type THEAD-TDOBJECT
      !TEXT type STRING
    returning
      value(EV_FAILED) type BOOLEAN .
  class-methods GET_MAT_CHARACTERISTICS
    importing
      !IV_MATNR type MATNR
    exporting
      !ET_ALLOCVALUESNUM type TT_BAPI1003_ALLOC_VALUES_NUM
      !ET_ALLOCVALUESCHAR type TT_BAPI1003_ALLOC_VALUES_CHAR
      !ET_ALLOCVALUESCURR type TT_BAPI1003_ALLOC_VALUES_CURR
      !ET_RETURN type BAPIRET2_T .
  class-methods VALIDATE_EBR
    importing
      !IS_EBRHEADER type ZCL_ZEBR_FORM_MPC_EXT=>TS_PART_LOT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  class-methods SUBMIT_EBR
    importing
      !IS_EBRHEADER type ZCL_ZEBR_FORM_MPC_EXT=>TS_PART_LOT
      !IT_EBR_MAST_DATA type ZCL_ZEBR_FORM_DPC_EXT=>TT_ZEBR_T_MAST_DATA
    exporting
      !EV_PLANNING_ID type ZDE_PLANNING_ID
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  class-methods GET_BATCH_CHAR
    importing
      !IV_MATNR type MATNR
      !IV_CHARG type CHARG_D
    exporting
      !ET_ALLOCVALUESNUM type TT_BAPI1003_ALLOC_VALUES_NUM
      !ET_ALLOCVALUESCHAR type TT_BAPI1003_ALLOC_VALUES_CHAR
      !ET_ALLOCVALUESCURR type TT_BAPI1003_ALLOC_VALUES_CURR
      !ET_RETURN type BAPIRET2_T .
  class-methods SAVE_TEXT_CHAT
    importing
      !ID type THEAD-TDID
      !NAME type THEAD-TDNAME
      !OBJECT type THEAD-TDOBJECT
      !TEXT type STRING
    exporting
      !CHAT_TEXT type STRING
    returning
      value(EV_FAILED) type BOOLEAN .
  class-methods GET_FERT_CHARACTERISTICS
    importing
      !IV_MATNR type MATNR
    exporting
      !EV_PRODUCT_TYPE type CHAR10
      !ET_ALLOCVALUESNUM type TT_BAPI1003_ALLOC_VALUES_NUM
      !ET_ALLOCVALUESCHAR type TT_BAPI1003_ALLOC_VALUES_CHAR
      !ET_ALLOCVALUESCURR type TT_BAPI1003_ALLOC_VALUES_CURR
      !ET_RETURN type BAPIRET2_T .
  class-methods BUILD_CODE
    changing
      !CS_EBRHEADER type ZCL_ZEBR_FORM_MPC_EXT=>TS_PART_LOT .
  class-methods FIRST_BUILD
    changing
      !CS_EBRHEADER type ZCL_ZEBR_FORM_MPC_EXT=>TS_PART_LOT .
  class-methods FILL_MASTERDATA
    importing
      !IV_OUT_MTART type MTART
      !IV_REQID type ZEBR_T_HEADER-REQ_NO
      !IV_LOT_MATNR type MATNR
    changing
      !CT_STPO_BOM type ZTT_BOM_COM_LEVEL
      !CT_MASTER_STATUS type TT_ZEBR_T_MAST_DATA
      !CR_DEEP_ENTITY type ZCL_ZEBR_FORM_MPC_EXT=>TS_PART_LOT
      !CT_MAPL type TT_MAPL
      !CT_PLAS_PLPO type TT_PLAS_PLPO
      !CS_EBRLOTINFO type ZCL_ZEBR_FORM_MPC_EXT=>TS_EBRLOTINFO
      !CT_MCH1 type TT_MCH1
      !CRT_MTART type MD_RANGE_T_MTART
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  class-methods SUBMIT_EBR_SCM
    importing
      !IS_EBRHEADER type ZCL_ZEBR_FORM_MPC_EXT=>TS_PART_LOT
      !IT_EBR_MAST_DATA type ZCL_ZEBR_FORM_DPC_EXT=>TT_ZEBR_T_MAST_DATA
    exporting
      !EV_PLANNING_ID type ZDE_PLANNING_ID
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  class-methods VALIDATE_PLANNING_ID
    importing
      !IV_PLANNING_ID type ZSCM_LOAD_PLAN-PLANNING_ID
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  class-methods GET_PART_DETAILS
    importing
      !IV_MFRPN type MFRPN
    changing
      !CS_EBRHEADER type ZCL_ZEBR_FORM_MPC_EXT=>TS_PART_LOT .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_STREAM
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~DELETE_STREAM
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~UPDATE_STREAM
    redefinition .
protected section.

  methods EBRADDPARTSET_GET_ENTITYSET
    redefinition .
  methods EBRASSYFACSET_GET_ENTITYSET
    redefinition .
  methods EBRBILLCODESET_GET_ENTITY
    redefinition .
  methods EBRBILLCODESET_GET_ENTITYSET
    redefinition .
  methods EBRBNDNOSET_GET_ENTITY
    redefinition .
  methods EBRBUMPFACSET_GET_ENTITYSET
    redefinition .
  methods EBRCARRIERTYPESE_GET_ENTITYSET
    redefinition .
  methods EBRCHATSET_CREATE_ENTITY
    redefinition .
  methods EBRCHATSET_GET_ENTITY
    redefinition .
  methods EBRDCFACSET_GET_ENTITYSET
    redefinition .
  methods EBRDIEQTYSET_GET_ENTITY
    redefinition .
  methods EBRDISCARDREASON_GET_ENTITYSET
    redefinition .
  methods EBRDISCARDSET_CREATE_ENTITY
    redefinition .
  methods EBRDISCARDSET_GET_ENTITY
    redefinition .
  methods EBRDSHIPFACSET_GET_ENTITYSET
    redefinition .
  methods EBRFASTTRACKSET_GET_ENTITYSET
    redefinition .
  methods EBRFILESET_GET_ENTITY
    redefinition .
  methods EBRFILESET_GET_ENTITYSET
    redefinition .
  methods EBRFINISHFACSET_GET_ENTITYSET
    redefinition .
  methods EBRGETFACILITYSE_GET_ENTITYSET
    redefinition .
  methods EBRHEADERSET_CREATE_ENTITY
    redefinition .
  methods EBRHEADERSET_GET_ENTITY
    redefinition .
  methods EBRHEADERSET_GET_ENTITYSET
    redefinition .
  methods EBRLOTINFOSET_GET_ENTITY
    redefinition .
  methods EBRLOTINFOSET_GET_ENTITYSET
    redefinition .
  methods EBRLOTSEARCHSET_GET_ENTITYSET
    redefinition .
  methods EBRMASTERSTATUSS_GET_ENTITYSET
    redefinition .
  methods EBRMATSEARCHSET_GET_ENTITY
    redefinition .
  methods EBRMATSEARCHSET_GET_ENTITYSET
    redefinition .
  methods EBRMFRPNSET_GET_ENTITY
    redefinition .
  methods EBRNOTIFICATIONS_GET_ENTITYSET
    redefinition .
  methods EBRNPPDETSET_GET_ENTITYSET
    redefinition .
  methods EBRPARTINFOSET_CREATE_ENTITY
    redefinition .
  methods EBRPARTINFOSET_GET_ENTITY
    redefinition .
  methods EBRPARTINFOSET_GET_ENTITYSET
    redefinition .
  methods EBRPBSFACSET_GET_ENTITYSET
    redefinition .
  methods EBRPCNTYPESET_GET_ENTITYSET
    redefinition .
  methods EBRPRFSET_GET_ENTITYSET
    redefinition .
  methods EBRRELSET_GET_ENTITYSET
    redefinition .
  methods EBRREQTYPESET_GET_ENTITY
    redefinition .
  methods EBRREQTYPESET_GET_ENTITYSET
    redefinition .
  methods EBRSENDBACKSET_CREATE_ENTITY
    redefinition .
  methods EBRSHIPTOSET_GET_ENTITYSET
    redefinition .
  methods EBRSTATUSLOGSET_GET_ENTITYSET
    redefinition .
  methods EBRTESTFACSET_GET_ENTITYSET
    redefinition .
  methods EBRVENDORSET_GET_ENTITYSET
    redefinition .
  methods EBRWAFERNOSET_GET_ENTITYSET
    redefinition .
  methods EBRWAFERSIZESET_GET_ENTITY
    redefinition .
  methods EBRWAFERSORTSET_GET_ENTITYSET
    redefinition .
  methods EBRREQTYPEVALUES_GET_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZEBR_FORM_DPC_EX IMPLEMENTATION.


  METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY.

    DATA : lr_deep_entity        TYPE zcl_zebr_form_mpc_ext=>ts_part_lot,
           ls_header             TYPE zebr_t_header,
           lt_header             TYPE STANDARD TABLE OF zebr_t_header,
           ls_lotinfo            TYPE zebr_t_lotinfo,
           lt_lotinfo            TYPE STANDARD TABLE OF zebr_t_lotinfo,
           lv_reqid              TYPE zebr_t_header-req_no,
           lv_reqid1             TYPE zebr_t_header-req_no,
           lt_notif              TYPE STANDARD TABLE OF zebr_t_notif,
           lv_name               TYPE thead-tdname,
           lo_exception          TYPE REF TO /iwbep/cx_mgw_tech_exception,
           lo_ebr_sucess         TYPE REF TO /iwbep/cx_mgw_tech_exception,
           lt_stat_log           TYPE STANDARD TABLE OF zebr_t_stat_log,
           lt_master_status      TYPE STANDARD TABLE OF  zebr_t_mast_data,
           lt_stpo_route         TYPE  ztt_bom_com_level,
           lt_stpo_bom           TYPE ztt_bom_com_level,
           lt_stpo_bom_srt       TYPE ztt_bom_com_level,
           lt_stpo_bom_fab       TYPE ztt_bom_com_level,
           lt_stpo_bom_pbs       TYPE ztt_bom_com_level,
           lt_stpo_bom_b1_b2     TYPE ztt_bom_com_level,
           lv_mfrpn              TYPE  string,
           lt_allocvaluesnum     TYPE TABLE OF  bapi1003_alloc_values_num,
           lt_allocvalueschar    TYPE  TABLE OF bapi1003_alloc_values_char,
           lt_allocvaluescurr    TYPE  TABLE OF bapi1003_alloc_values_curr,
           lt_allocations        TYPE TABLE OF api_kssk,
           lt_return             TYPE TABLE OF bapiret2,
           lt_bom_details        TYPE ztt_bom_item,
           lt_stpo               TYPE ztt_bom_com_level,
           lt_snum_asy           TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
           lt_scurr_asy          TYPE STANDARD TABLE OF bapi1003_alloc_values_curr,
           lt_schar_asy          TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
           lt_return_asy         TYPE bapiret2_t,
           lt_bom_output         TYPE ztt_bom_ip_out,
           lv_asy_lifnr          TYPE lifnr,
           lv_waferqty           TYPE i,
           lv_product_type       TYPE char10,
           lv_package_enginer    TYPE uname,
           lv_sort_facility      TYPE lifnr,
           lv_pbs_facility       TYPE lifnr,
           lv_final_tst_facility TYPE lifnr,
           lv_finish_facility    TYPE lifnr,
           lv_srt_plnnr          TYPE plnnr,
           lv_pbs_plnnr          TYPE plnnr,
           lv_tst_plnnr          TYPE plnnr,
           lv_fert_plnnr         TYPE plnnr,
           lv_srt_plnal          TYPE plnal,
           lv_pbs_plnal          TYPE plnal,
           lv_tst_plnal          TYPE plnal,
           lv_fert_plnal         TYPE plnal,
           lv_planid             TYPE zscm_load_plan-planning_id,
           lv_fert_pbs           TYPE boolean,
           lv_lot_matnr          TYPE matnr,
           lt_mapl               TYPE STANDARD TABLE OF ty_mapl,
           lt_plas_plpo          TYPE STANDARD TABLE OF ty_plas_plpo,
           lv_werks_mpw          TYPE werks_d,
           lv_matnr_fab          TYPE matnr,
           lv_stlal_lot          TYPE stlal,
           lv_srt_plnnr_alt      TYPE plko-plnnr_alt,
           lv_pbs_plnnr_alt      TYPE plko-plnnr_alt,
           lv_tst_plnnr_alt      TYPE plko-plnnr_alt,
           lv_fert_plnnr_alt     TYPE plko-plnnr_alt,
           lrt_mtart             TYPE RANGE OF  mtart.
    CLEAR: lv_reqid,ls_header,lt_header[],lr_deep_entity ,ls_lotinfo,lt_lotinfo[],lv_reqid1 ,lt_mapl , lt_plas_plpo ,
           ls_header   , lt_header[],ls_lotinfo ,lt_lotinfo[],lv_reqid  ,  lv_reqid1  ,  lt_notif    ,lv_name  ,
           lo_exception  , lo_ebr_sucess , lt_stat_log  , lt_master_status  ,lt_stpo_route  , lt_stpo_bom   ,
           lt_stpo_bom_srt  ,lt_stpo_bom_fab ,   lt_stpo_bom_pbs , lt_stpo_bom_pbs ,  lt_stpo_bom_b1_b2  , lv_mfrpn  ,
           lt_allocvaluesnum ,lt_allocvalueschar ,lt_allocvaluescurr ,lt_allocations  ,lt_return  ,lt_bom_details ,
           lt_stpo  ,lt_snum_asy   , lt_scurr_asy  ,lt_schar_asy  ,lt_return_asy   , lt_bom_output  ,lv_asy_lifnr  ,
           lv_waferqty ,lv_product_type  ,lv_package_enginer , lv_sort_facility   ,lv_pbs_facility  ,lv_final_tst_facility,
           lv_finish_facility , lv_srt_plnnr   ,  lv_pbs_plnnr , lv_tst_plnnr  ,lv_fert_plnnr   ,lv_srt_plnal , lv_pbs_plnal ,
           lv_tst_plnal  ,lv_fert_plnal ,lv_planid   ,lv_fert_pbs ,lt_mapl   , lt_plas_plpo  , lv_matnr_fab  ,lv_lot_matnr,
           lv_stlal_lot .


    " add DC facilities from
    SELECT * FROM zebr_config  INTO TABLE @DATA(lt_zebr_config_dc)  WHERE name  = 'DESIGN_CENTER' "#EC CI_ALL_FIELDS_NEEDED
                                                                        AND type = 'DROP_DOWN'  .


    " get data from screen
    io_data_provider->read_entry_data(
    IMPORTING
    es_data = lr_deep_entity ).

    IF lr_deep_entity IS INITIAL  .
      CREATE OBJECT lo_exception.
      IF  lo_exception IS BOUND.
        lo_exception->get_msg_container( )->add_message(
           EXPORTING
             iv_msg_type     = 'E'
             iv_msg_id      = '00'
             iv_msg_number  = '123'
             iv_msg_text    = 'No input received to backend' ).
*          IV_MSG_V1
        RAISE EXCEPTION lo_exception.
        RETURN.
      ENDIF.
    ENDIF.

    IF lr_deep_entity-req_status_no  = '00'  AND lr_deep_entity-target_status_no  =  '01' .
      " error message when fast track is 03
      IF lr_deep_entity-fasttrack_no   =  '03'  OR   lr_deep_entity-fasttrack_no   =  '3' .
        SELECT  SINGLE  req_no  FROM zebr_t_file  INTO @DATA(ls_zebr_t_file_ft)  WHERE req_no  =  @lr_deep_entity-req_no .
        IF sy-subrc NE  0 .
          DATA    lo_wf_exception_ft  TYPE REF TO  /iwbep/cx_mgw_tech_exception   .
          CREATE OBJECT lo_wf_exception_ft .
          IF lo_wf_exception_ft IS  BOUND .
            lo_wf_exception_ft->get_msg_container( )->add_message(
                     EXPORTING
                       iv_msg_type    = 'I'
                       iv_msg_id      = '00'
                       iv_msg_number  = '123'
                       iv_msg_text    = 'Need approval for any Handcarry support from Ron/Xuejun/Michael' ).
            RAISE EXCEPTION lo_wf_exception_ft .
            RETURN .
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    SELECT SINGLE  field2  FROM zebr_config  INTO lr_deep_entity-req_status   WHERE name    = 'REQ_STATUS'
                                                                                AND type    =  'DROP_DOWN'
                                                                                AND field1  = lr_deep_entity-req_status_no .

    SELECT SINGLE  field2  FROM zebr_config  INTO lr_deep_entity-target_status   WHERE name  = 'REQ_STATUS'
                                           AND type  =  'DROP_DOWN'
                                           AND field1  = lr_deep_entity-target_status_no .


    IF lr_deep_entity-req_no NE '*' .
      lv_reqid  = lr_deep_entity-req_no .
      " get requestor  and group
      SELECT SINGLE * FROM zebr_t_header INTO CORRESPONDING FIELDS OF ls_header WHERE req_no  =  lv_reqid .
      IF sy-subrc EQ 0 AND ls_header-req_status_no   NE lr_deep_entity-req_status_no   .
        DATA    lo_wf_exception_stat  TYPE REF TO  /iwbep/cx_mgw_tech_exception   .
        CREATE OBJECT lo_wf_exception_stat .
        IF lo_wf_exception_stat IS  BOUND .
          lo_wf_exception_stat->get_msg_container( )->add_message(
                   EXPORTING
                     iv_msg_type     = 'E'
                     iv_msg_id      = '00'
                     iv_msg_number  = '123'
                     iv_msg_text    = 'EBR is already processed by ' && ls_header-changed_by && ' '  && ',Refresh EBR  to get updated status' ).
          RAISE EXCEPTION lo_wf_exception_stat .
          RETURN .
        ENDIF.

      ENDIF.
      SELECT SINGLE bname , accnt   FROM usr02  INTO @DATA(ls_usr)  WHERE bname = @sy-uname .
      IF ls_usr IS NOT INITIAL .
        SELECT  SINGLE employee_id ,displayname , maximorg  FROM zworkday_data  INTO @DATA(ls_workday) WHERE  employee_id = @ls_usr-accnt .
        ls_header-changed_by         = sy-uname . "ls_workday-displayname  .
        ls_header-changed_on         = sy-datum .
        ls_header-changed_time       = sy-uzeit .
        lr_deep_entity-changed_by    = ls_workday-displayname  .
        lr_deep_entity-changed_on    = sy-datum .
        lr_deep_entity-changed_time  = sy-uzeit .

      ENDIF .

      IF lr_deep_entity-req_status_no  NE lr_deep_entity-target_status_no .
        APPEND INITIAL LINE TO  lt_stat_log ASSIGNING FIELD-SYMBOL(<fs_stat>)  .
        <fs_stat>-req_no               = lv_reqid .
        <fs_stat>-req_status_no        = lr_deep_entity-target_status_no .
        <fs_stat>-req_status           = lr_deep_entity-target_status  .
        <fs_stat>-created_by           = sy-uname .
        <fs_stat>-created_at           = sy-uzeit .
        <fs_stat>-created_on           = sy-datum .
        DATA(lv_req_status_no)         =  lr_deep_entity-req_status_no  .
        DATA(lv_target_status_no)      =  lr_deep_entity-target_status_no .
        lr_deep_entity-req_status_no   = lr_deep_entity-target_status_no .
        lr_deep_entity-req_status      = lr_deep_entity-target_status  .

      ENDIF.
    ENDIF.

    " header                         .
    ls_header-req_type                             =   lr_deep_entity-req_type                                 .
    ls_header-req_type_desc                        =   lr_deep_entity-req_type_desc                            .
    lv_mfrpn                                       =   lr_deep_entity-mfrpn                            .
    CALL METHOD cl_http_utility=>unescape_url
      EXPORTING
        escaped   = lv_mfrpn
      RECEIVING
        unescaped = lr_deep_entity-mfrpn.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = lr_deep_entity-mfrpn
      IMPORTING
        output       = lr_deep_entity-output_part
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF lr_deep_entity-target_status_no IS NOT INITIAL ." AND lr_deep_entity-target_status IS NOT INITIAL  .
      ls_header-req_status_no                        =   lr_deep_entity-target_status_no                            .
      ls_header-req_status                           =   lr_deep_entity-target_status                               .
    ELSE.
      ls_header-req_status_no                        =   lr_deep_entity-req_status_no                            .
      ls_header-req_status                           =   lr_deep_entity-req_status                               .
    ENDIF.
    ls_header-mpw_wafer                            =   lr_deep_entity-mpw_wafer                                .
    ls_header-hybrid                               =   lr_deep_entity-hybrid .
    ls_header-output_part                          =   lr_deep_entity-output_part  .

    ls_header-mfrpn                                =   lr_deep_entity-mfrpn                            .
    ls_header-status                               =   lr_deep_entity-status                                   .
    ls_header-group_ebr                            =   lr_deep_entity-group_ebr                                .
    ls_header-bu_name                              =   lr_deep_entity-bu_name                                  .
    ls_header-package_type                         =   lr_deep_entity-package_type                             .
    ls_header-package_code                         =   lr_deep_entity-package_code                             .
    ls_header-die_type                             =   lr_deep_entity-die_type                                 .
    ls_header-dierevcode                           =   lr_deep_entity-dierevcode                               .
    ls_header-sample_marking                       =   lr_deep_entity-sample_marking                           .
    ls_header-additional_marking                   =   lr_deep_entity-additional_marking                       .
    ls_header-wafer_sort                           =   lr_deep_entity-wafer_sort                               .
    ls_header-ship_to                              =   lr_deep_entity-ship_to                                  .
    ls_header-nprf_no                              =   lr_deep_entity-nprf_no                                  .
    ls_header-nfo                                  =   lr_deep_entity-nfo                                      .
    ls_header-swr                                  =   lr_deep_entity-swr                                      .
    ls_header-fasttrack                            =   lr_deep_entity-fasttrack                                .
    ls_header-fasttrack_no                         =   lr_deep_entity-fasttrack_no                             .
    ls_header-drop_ship                            =   lr_deep_entity-drop_ship                                .
    ls_header-dc_location                          =   lr_deep_entity-dc_location                              .
    ls_header-assy_instr                           =   lr_deep_entity-assy_instr                               .
*    ls_header-npp_processor                        =   lr_deep_entity-npp_processor                            .

    ls_header-assy_carrier_type                    =   lr_deep_entity-assy_carrier_type                        .
    ls_header-test_carrier_type                    =   lr_deep_entity-test_carrier_type                        .
    ls_header-req_final                            =   lr_deep_entity-req_final                                .
    ls_header-req_pcn                              =   lr_deep_entity-req_pcn                                  .
    ls_header-req_rel                              =   lr_deep_entity-req_rel                                  .
    ls_header-apr_no                               =   lr_deep_entity-apr_no                                   .
    ls_header-pcn_no                               =   lr_deep_entity-pcn_no                                   .
    ls_header-relid                                =   lr_deep_entity-relid                                    .  " rel id detrmination  is based on lots this will be over written  when maitinaed in  batch characteristics

    ls_header-pre_qual                             =   lr_deep_entity-pre_qual                                      .
    ls_header-pre_qual_ft                          =   lr_deep_entity-pre_qual_ft                                   .
    ls_header-assy_buy_off                         =   lr_deep_entity-assy_buy_off                                  .
    ls_header-assy_ft_reqd                         =   lr_deep_entity-assy_ft_reqd                                  .
    ls_header-pcn_type                             =   lr_deep_entity-pcn_type .
    ls_header-discard_reason                       =   lr_deep_entity-discard_reason .
    ls_header-lot_in_db                            = lr_deep_entity-lot_in_db .

    IF lr_deep_entity-ws_out_date IS NOT INITIAL.
      ls_header-ws_out_date = lr_deep_entity-ws_out_date.
    ENDIF.
    IF lr_deep_entity-issue_assy_team IS NOT INITIAL   AND  ls_header-issue_assy_team  IS INITIAL .
      ls_header-issue_assy_set_date                  =   sy-datum                   .
      CLEAR  :  ls_header-issue_assy_com_date  .
    ENDIF.
    IF lr_deep_entity-issue_assy_team IS INITIAL   AND  ls_header-issue_assy_team  IS NOT INITIAL .
      ls_header-issue_assy_com_date                  =   sy-datum                   .
    ENDIF.                         .
    IF lr_deep_entity-issue_pdm_team IS NOT INITIAL   AND ls_header-issue_pdm_team IS INITIAL .
      ls_header-issue_pdm_set_date                   =   sy-datum                   .
      CLEAR  ls_header-issue_pdm_com_date .
    ENDIF.
    IF lr_deep_entity-issue_pdm_team IS INITIAL   AND ls_header-issue_pdm_team IS NOT INITIAL .
      ls_header-issue_pdm_com_date                   =   sy-datum                   .
    ENDIF.
    ls_header-issue_assy_team                      =   lr_deep_entity-issue_assy_team                          .
    ls_header-issue_pdm_team                       =   lr_deep_entity-issue_pdm_team                           .
    ls_header-masterdata_status                    =   lr_deep_entity-masterdata_status                        .
    ls_header-supply_status                        =   lr_deep_entity-supply_status                            .
    ls_header-add_mark_char                        =   lr_deep_entity-add_mark_char  .
    ls_header-qa_buy_off                           =   lr_deep_entity-qa_buy_off  .
    ls_header-lot_induct                           =   lr_deep_entity-lot_induct .
    ls_header-ship_to_chng                         =  lr_deep_entity-ship_to_chng             .
    ls_header-wf_chng_flag                         =  lr_deep_entity-wf_chng_flag             .
    ls_header-carr_chng_flag                       =  lr_deep_entity-carr_chng_flag           .
    ls_header-new_bom                              =  lr_deep_entity-new_bom  .
    ls_header-new_bnd                              =  lr_deep_entity-new_bnd  .
    ls_header-doe                                  =  lr_deep_entity-doe      .
    ls_header-add_spec_top                         =  lr_deep_entity-add_spec_top  .
    ls_header-att_map                              =  lr_deep_entity-att_map       .
    ls_header-skip_skelton                         =  lr_deep_entity-skip_skelton  .
    ls_header-split_lot                            =  lr_deep_entity-split_lot     .
    ls_header-npi_check                            =  lr_deep_entity-npi_check     .
    ls_header-swr_sar                              =  lr_deep_entity-swr_sar       .
    ls_header-krpw_flag                           =  lr_deep_entity-krpw_flag     .
    ls_header-thin_wlp_flag                        =  lr_deep_entity-thin_wlp_flag .
    " adjust   lifnr  and plnnr and plnal


    " first build
    CALL METHOD zcl_zebr_form_dpc_ext=>first_build
      CHANGING
        cs_ebrheader = lr_deep_entity.

    " bill code
    CALL METHOD zcl_zebr_form_dpc_ext=>build_code
      CHANGING
        cs_ebrheader = lr_deep_entity.

    ls_header-bill_code  =  lr_deep_entity-bill_code .

    " default values
    lr_deep_entity-wf_chng_flag  =  'SORT'  .
    lr_deep_entity-ship_to_chng  =  'MPOC'  .

    SELECT SINGLE matnr ,mtart , bismt ,mfrpn FROM mara INTO  @DATA(ls_mara)  WHERE mfrpn = @lr_deep_entity-mfrpn .
    IF sy-subrc EQ 0.
      DATA(lv_out_mtart)  = ls_mara-mtart .
      IF ls_mara-mtart  = 'FERT'.
        DATA(lv_fert_matnr)   = ls_mara-matnr .
        " if out material is fert and fert route is selected  than pick the right fert alternate to explode bom
        READ TABLE  lr_deep_entity-ebrheadlotnav   INTO  DATA(ls_lotinfo_fert)  INDEX 1 .
        IF sy-subrc EQ 0  AND  ls_lotinfo_fert-finish_facility  IS NOT INITIAL .
          IF lr_deep_entity-mpw_wafer  IS NOT INITIAL .
            CLEAR lv_werks_mpw .
            lv_werks_mpw  =  ls_lotinfo_fert-finish_facility .
            SELECT a~matnr ,
                  a~werks  ,
                  a~plnty  ,
                  a~plnnr  ,
                  a~plnal  ,
                  b~plnnr_alt INTO TABLE @DATA(lt_mapl_fert)  FROM mapl   AS a  INNER JOIN plko  AS b ON    a~plnty  =   b~plnty
                                                                                            AND   a~plnnr  =   b~plnnr
                                                                                            AND   a~plnal  =   b~plnal
*                                                                                       FOR ALL ENTRIES IN lt_stpo_bom "#EC CI_NO_TRANSFORM
                                                                                        WHERE a~matnr  = @lv_fert_matnr
                                                                                         AND  a~werks  = @lv_werks_mpw
                                                                                         AND  a~plnty  = 'N'
                                                                                         AND  b~verwe   NE 'CST' "IN ('PRD','INI','NPR')
                                                                                         AND  a~loekz   EQ  ' '.
          ELSE.
            SELECT a~matnr
                a~werks
                a~plnty
                a~plnnr
                a~plnal
                b~plnnr_alt INTO TABLE lt_mapl_fert  FROM mapl   AS a  INNER JOIN plko  AS b ON    a~plnty  =   b~plnty
                                                                                          AND   a~plnnr  =   b~plnnr
                                                                                          AND   a~plnal  =   b~plnal
*                                                                                       FOR ALL ENTRIES IN lt_stpo_bom "#EC CI_NO_TRANSFORM
                                                                                      WHERE a~matnr  = lv_fert_matnr
*                                                                                         AND  a~werks  = @lv_werks_mpw
                                                                                       AND  a~plnty  = 'N'
                                                                                       AND  a~plnnr  = ls_lotinfo_fert-finish_facility+0(8)
                                                                                       AND  a~plnal  = ls_lotinfo_fert-finish_facility+8(2)
                                                                                       AND  b~verwe   NE 'CST' "IN ('PRD','INI','NPR')
                                                                                       AND  a~loekz   EQ  ' '.
          ENDIF.
          IF lt_mapl_fert IS NOT INITIAL.
            SELECT a~plnty  ,                      "#EC CI_NO_TRANSFORM
                   a~plnnr  ,
                   a~plnal  ,
                   a~plnkn  ,
                   a~zaehl  ,
                   b~datuv  ,
                   b~loekz  ,
                   b~vornr  ,
                   b~werks  ,
                   b~lifnr  ,
                   b~infnr   INTO TABLE @DATA(lt_plas_plpo_mpw)  FROM plas AS a  INNER JOIN plpo AS b
                                                    ON    a~plnty   =   b~plnty
                                                   AND    a~plnnr   =   b~plnnr
                                                   AND    a~plnkn   =   b~plnkn
                                         FOR ALL ENTRIES IN  @lt_mapl_fert
                                                 WHERE  a~plnty  =  @lt_mapl_fert-plnty
                                                  AND   a~plnnr  =  @lt_mapl_fert-plnnr
                                                  AND   a~plnal  =  @lt_mapl_fert-plnal
                                                  AND   b~werks  =  @lt_mapl_fert-werks .
            IF sy-subrc EQ 0.
              SORT lt_plas_plpo_mpw  BY plnty plnnr plnal ASCENDING datuv  DESCENDING  .
              DELETE ADJACENT DUPLICATES FROM lt_plas_plpo_mpw   COMPARING plnty plnnr plnal vornr .
              DELETE lt_plas_plpo_mpw WHERE loekz  =  'X'  .
              READ TABLE lt_plas_plpo_mpw INTO DATA(ls_plas_plpo_mpw)  INDEX 1 .
              IF sy-subrc EQ 0.
                DATA(lv_fert_plnnr_det)   = ls_plas_plpo_mpw-plnnr .
                DATA(lv_fert_plnal_det)   = ls_plas_plpo_mpw-plnal .
                lv_werks_mpw              = ls_plas_plpo_mpw-werks .
              ENDIF.
            ENDIF.
          ENDIF.
*          ELSE.
*            " other tha mpw  we are storing route in teh facility
*            lv_werks_mpw        =
*            lv_fert_plnnr_det   = ls_lotinfo_fert-finish_facility+0(8) .
*            lv_fert_plnal_det   = ls_lotinfo_fert-finish_facility+8(2) .
        ENDIF.
        IF lv_fert_plnnr_det IS NOT INITIAL AND lv_fert_plnal_det IS NOT INITIAL .
          " determine right fert alternate
          SELECT SINGLE matnr  ,
                        werks  ,
                        verid  ,
                        stlal  ,
                        stlan  ,
                        plnty  ,
                        plnnr  ,
                        alnal   FROM   mkal   INTO @DATA(ls_mkal)  WHERE matnr = @lv_fert_matnr
                                                                           AND plnnr = @lv_fert_plnnr_det
                                                                           AND alnal = @lv_fert_plnal_det .
          IF sy-subrc EQ 0.
            DATA(lv_stlal)  = ls_mkal-stlal .
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.

      CALL FUNCTION 'ZGET_MULTILEVEL_BOM_BOTTOM_UP'
        EXPORTING
          im_matnr         = ls_mara-matnr
        CHANGING
          ct_bom_details   = lt_bom_details
          ct_bom_input_out = lt_bom_output.


      DELETE lt_bom_details  WHERE zout_mtart NE 'FERT'  .
      DELETE lt_bom_output    WHERE zout_mtart NE 'FERT'  .
      SELECT matnr , matkl FROM mara INTO TABLE @DATA(lt_mara_matgrp)
                FOR ALL ENTRIES IN @lt_bom_output  WHERE matnr  = @lt_bom_output-zout_mat .
      IF sy-subrc EQ 0.
        DELETE  lt_mara_matgrp WHERE ( matkl  = 'F005'  OR matkl = 'F009'  OR matkl  = 'F010'  )  .
      ENDIF.
      READ TABLE lt_mara_matgrp  INTO DATA(ls_mara_matgrp) INDEX 1  .
      IF sy-subrc EQ  0.
        " get the fert material to use further  for find characteristics
        lv_fert_matnr   = ls_mara_matgrp-matnr.
      ENDIF.

    ENDIF.
    IF lv_stlal IS  INITIAL  .
      CALL METHOD zcl_bom_explode_util=>bom_explode
        EXPORTING
          iv_matnr              = lv_fert_matnr
          iv_werks              = '4900'
*         ls_stpo               =
          iv_get_marterial_type = 'X'
          iv_active_bom_01      = 'X'
*         IV_USAGE_PROBABILITY  = 'X'
        CHANGING
          it_stpo               = lt_stpo.
    ELSE.
      CALL METHOD zcl_bom_explode_util=>bom_explode
        EXPORTING
          iv_matnr              = lv_fert_matnr
          iv_werks              = '4900'
*         ls_stpo               =
          iv_get_marterial_type = 'X'
*         iv_active_bom         =
          iv_bom_alt            = lv_stlal
          iv_active_bom_01      = 'X'
*         IV_USAGE_PROBABILITY  = 'X'
        CHANGING
          it_stpo               = lt_stpo.
      DATA(lt_stpo_mtart)  = lt_stpo .
      SORT lt_stpo_mtart  BY mtart .
      DELETE ADJACENT DUPLICATES FROM  lt_stpo_mtart COMPARING mtart .
      LOOP AT  lt_stpo_mtart  INTO DATA(ls_stpo_mtart)  .
        APPEND INITIAL LINE TO lrt_mtart ASSIGNING FIELD-SYMBOL(<fs_mtart>) .
        <fs_mtart>-sign    = 'I'  .
        <fs_mtart>-option  = 'EQ'  .
        <fs_mtart>-low     = ls_stpo_mtart-mtart  .
      ENDLOOP.

    ENDIF.

    " get assembly characterostics
    DATA(lt_asy)   = lt_stpo   .
    DELETE lt_asy WHERE mtart NE 'ZASY'  .
    READ TABLE lt_asy  INTO DATA(ls_asy)  INDEX 1  .
    IF sy-subrc EQ 0.
      CALL METHOD zcl_zebr_form_dpc_ext=>get_mat_characteristics
        EXPORTING
          iv_matnr           = ls_asy-matnr
        IMPORTING
          et_allocvaluesnum  = lt_snum_asy
          et_allocvalueschar = lt_schar_asy
          et_allocvaluescurr = lt_scurr_asy
          et_return          = lt_return_asy.
      READ TABLE lt_schar_asy INTO DATA(ls_schar_asy)  WITH KEY charact = 'CARRIERTYPE' .
      IF sy-subrc EQ 0.
        lr_deep_entity-carr_chng_flag   = ls_schar_asy-value_char .
      ENDIF.
    ENDIF.



    " get allbatch sort material from  for all lot's
    IF  lr_deep_entity-ebrheadlotnav    IS NOT INITIAL .
      SELECT  a~matnr, a~charg  ,a~licha , b~mtart INTO TABLE @DATA(lt_mch1)
                                             FROM   mch1  AS a INNER JOIN mara   AS b
                                             ON a~matnr  = b~matnr
                                             FOR ALL ENTRIES IN  @lr_deep_entity-ebrheadlotnav
                                             WHERE a~licha   EQ  @lr_deep_entity-ebrheadlotnav-lot_no
                                             AND   b~mtart    IN ('ZSRT','ZFAB') .
      IF sy-subrc EQ 0.


      ENDIF.
    ENDIF.
    LOOP AT  lr_deep_entity-ebrheadlotnav   ASSIGNING FIELD-SYMBOL(<fs_ebrlotinfo>)  .
      <fs_ebrlotinfo>-req_no  = lv_reqid  .

      lv_mfrpn   =  <fs_ebrlotinfo>-mfrpn .
      " conversion
      CALL METHOD cl_http_utility=>unescape_url
        EXPORTING
          escaped   = lv_mfrpn
        RECEIVING
          unescaped = <fs_ebrlotinfo>-mfrpn.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = <fs_ebrlotinfo>-mfrpn
        IMPORTING
          output       = <fs_ebrlotinfo>-output_part
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      " fab out date   if lot is  fab lot   since same lot  no can be used for fab and sort
      " first read sort and than if not avialable go for fab
      READ TABLE lt_mch1  INTO DATA(ls_mch1)  WITH KEY licha  = <fs_ebrlotinfo>-lot_no
                                                       mtart  =  'ZSRT'  .
      IF sy-subrc NE 0.
        " fab out date   if lot is  fab lot   since same lot  no can be used for fab and sort
        READ TABLE lt_mch1  INTO ls_mch1  WITH KEY licha  = <fs_ebrlotinfo>-lot_no
                                                   mtart  =  'ZFAB'  .
        IF sy-subrc EQ 0.
          lv_lot_matnr  = ls_mch1-matnr .
          "
          SELECT  ebeln  ,ebelp  , eindt  FROM   eket INTO TABLE @DATA(lt_eket)  WHERE licha  = @ls_mch1-licha
                                                                                 AND  charg  = @ls_mch1-charg .
          IF sy-subrc EQ 0.
            SORT lt_eket BY  eindt .
            READ TABLE lt_eket INTO DATA(ls_eket)  INDEX  1 .
            IF sy-subrc EQ 0.
              <fs_ebrlotinfo>-fab_out_date   =  ls_eket-eindt .
            ENDIF.
          ENDIF.

          REFRESH  lt_eket .
        ENDIF.
      ELSE.
        lv_lot_matnr  = ls_mch1-matnr .
        " fab out date   if lot is  fab lot   since same lot  no can be used for fab and sort
        READ TABLE lt_mch1  INTO ls_mch1  WITH KEY licha  = <fs_ebrlotinfo>-lot_no
                                                   mtart  =  'ZFAB'  .
        IF sy-subrc EQ 0.          "
          SELECT  ebeln  ebelp   eindt  FROM   eket INTO TABLE lt_eket  WHERE licha  = ls_mch1-licha
                                                                                 AND  charg  = ls_mch1-charg .
          IF sy-subrc EQ 0.
            SORT lt_eket BY  eindt .
            READ TABLE lt_eket INTO ls_eket  INDEX  1 .
            IF sy-subrc EQ 0.
              <fs_ebrlotinfo>-fab_out_date   =  ls_eket-eindt .
              CLEAR ls_eket .
            ENDIF.
          ENDIF.

          REFRESH  lt_eket .
        ENDIF.
      ENDIF.

      AT NEW output_part  .
        REFRESH  :  lt_stpo_bom  ,
                    lt_stpo_route .
        CLEAR  : lv_sort_facility ,
                 lv_pbs_facility ,
                 lv_final_tst_facility ,
                 lv_finish_facility,
                 lv_srt_plnnr     ,
                 lv_pbs_plnnr    ,
                 lv_tst_plnnr    ,
                 lv_fert_plnnr   ,
                 lv_srt_plnal    ,
                 lv_pbs_plnal    ,
                 lv_tst_plnal    ,
                 lv_fert_plnal    ,
                 lv_srt_plnnr_alt ,
                 lv_pbs_plnnr_alt ,
                 lv_tst_plnnr_alt ,
                 lv_stlal_lot,
                 lv_fert_plnnr_alt .

        SELECT SINGLE matnr ,mtart , bismt ,mfrpn FROM mara INTO  @DATA(ls_mara_lot)  WHERE matnr  =  @<fs_ebrlotinfo>-output_part .
        IF sy-subrc EQ 0.
          DATA(lv_out_mtart_lot)  = ls_mara_lot-mtart .
          IF ls_mara-mtart  = 'FERT'.
            DATA(lv_fert_matnr_lot)   = ls_mara_lot-matnr .
            " if out material is fert and fert route is selected  than pick the right fert alternate to explode bom

            IF <fs_ebrlotinfo>-finish_facility  IS NOT INITIAL .
              IF lr_deep_entity-mpw_wafer  IS NOT INITIAL .
                CLEAR lv_werks_mpw .
                lv_werks_mpw  =  <fs_ebrlotinfo>-finish_facility .
                SELECT a~matnr ,
                      a~werks  ,
                      a~plnty  ,
                      a~plnnr  ,
                      a~plnal  ,
                      b~datuv  ,
                      b~statu  ,
                      b~plnnr_alt INTO TABLE @DATA(lt_mapl_fert_lot)  FROM mapl   AS a  INNER JOIN plko  AS b ON    a~plnty  =   b~plnty
                                                                                                AND   a~plnnr  =   b~plnnr
                                                                                                AND   a~plnal  =   b~plnal
*                                                                                       FOR ALL ENTRIES IN lt_stpo_bom "#EC CI_NO_TRANSFORM
                                                                                            WHERE a~matnr  = @lv_fert_matnr
                                                                                             AND  a~werks  = @lv_werks_mpw
                                                                                             AND  a~plnty  = 'N'
                                                                                             AND  b~verwe   NE 'CST' "IN ('PRD','INI','NPR')
                                                                                             AND  a~loekz   EQ  ' '.
              ELSE.
                SELECT a~matnr
                    a~werks
                    a~plnty
                    a~plnnr
                    a~plnal
                     b~datuv
                     b~statu
                    b~plnnr_alt INTO TABLE lt_mapl_fert_lot  FROM mapl   AS a  INNER JOIN plko  AS b ON    a~plnty  =   b~plnty
                                                                                              AND   a~plnnr  =   b~plnnr
                                                                                              AND   a~plnal  =   b~plnal
*                                                                                       FOR ALL ENTRIES IN lt_stpo_bom "#EC CI_NO_TRANSFORM
                                                                                          WHERE a~matnr  = lv_fert_matnr_lot
*                                                                                         AND  a~werks  = @lv_werks_mpw
                                                                                           AND  a~plnty  = 'N'
                                                                                           AND  a~plnnr  = <fs_ebrlotinfo>-finish_facility+0(8)
                                                                                           AND  a~plnal  = <fs_ebrlotinfo>-finish_facility+8(2)
                                                                                           AND  b~verwe   NE 'CST' "IN ('PRD','INI','NPR')
                                                                                           AND  a~loekz   EQ  ' '.
              ENDIF.
              IF lt_mapl_fert_lot IS NOT INITIAL.
                SORT lt_mapl_fert_lot  BY plnnr plnal ASCENDING datuv DESCENDING .
                DELETE ADJACENT DUPLICATES FROM  lt_mapl_fert_lot  COMPARING plnnr plnal  .
                DELETE lt_mapl_fert_lot WHERE statu EQ  '5' .
                SELECT a~plnty  ,                  "#EC CI_NO_TRANSFORM
                       a~plnnr  ,
                       a~plnal  ,
                       a~plnkn  ,
                       a~zaehl  ,
                       b~datuv  ,
                       b~loekz  ,
                       b~vornr  ,
                       b~werks  ,
                       b~lifnr  ,
                       b~infnr   INTO TABLE @DATA(lt_plas_plpo_mpw_lot)  FROM plas AS a  INNER JOIN plpo AS b
                                                        ON    a~plnty   =   b~plnty
                                                       AND    a~plnnr   =   b~plnnr
                                                       AND    a~plnkn   =   b~plnkn
                                             FOR ALL ENTRIES IN  @lt_mapl_fert
                                                     WHERE  a~plnty  =  @lt_mapl_fert-plnty
                                                      AND   a~plnnr  =  @lt_mapl_fert-plnnr
                                                      AND   a~plnal  =  @lt_mapl_fert-plnal
                                                      AND   b~werks  =  @lt_mapl_fert-werks .
                IF sy-subrc EQ 0.
                  SORT lt_plas_plpo_mpw_lot  BY plnty plnnr plnal ASCENDING datuv  DESCENDING  .
                  DELETE ADJACENT DUPLICATES FROM lt_plas_plpo_mpw_lot   COMPARING plnty plnnr plnal vornr .
                  DELETE lt_plas_plpo_mpw_lot WHERE loekz  =  'X'  .
                  READ TABLE lt_plas_plpo_mpw_lot INTO DATA(ls_plas_plpo_mpw_lot)  INDEX 1 .
                  IF sy-subrc EQ 0.
                    DATA(lv_fert_plnnr_det_lot)   = ls_plas_plpo_mpw_lot-plnnr .
                    DATA(lv_fert_plnal_det_lot)   = ls_plas_plpo_mpw_lot-plnal .
                    DATA(lv_werks_mpw_lot)        = ls_plas_plpo_mpw_lot-werks .
                  ENDIF.
                ENDIF.
              ENDIF.
*          ELSE.
*            " other tha mpw  we are storing route in teh facility
*            lv_werks_mpw        =
*            lv_fert_plnnr_det   = ls_lotinfo_fert-finish_facility+0(8) .
*            lv_fert_plnal_det   = ls_lotinfo_fert-finish_facility+8(2) .
            ENDIF.
            IF lv_fert_plnnr_det_lot IS NOT INITIAL AND lv_fert_plnal_det_lot IS NOT INITIAL .
              " determine right fert alternate
              SELECT SINGLE matnr  ,
                            werks  ,
                            verid  ,
                            stlal  ,
                            stlan  ,
                            plnty  ,
                            plnnr  ,
                            alnal   FROM   mkal   INTO @DATA(ls_mkal_lot)  WHERE matnr = @lv_fert_matnr_lot
                                                                               AND plnnr = @lv_fert_plnnr_det_lot
                                                                               AND alnal = @lv_fert_plnal_det_lot .
              IF sy-subrc EQ 0.
                lv_stlal_lot  = ls_mkal_lot-stlal .
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        IF lv_stlal_lot IS NOT INITIAL .
          CALL METHOD zcl_bom_explode_util=>bom_explode
            EXPORTING
              iv_matnr              = <fs_ebrlotinfo>-output_part
              iv_werks              = '4900'
*             ls_stpo               =
              iv_get_marterial_type = 'X'
              iv_bom_alt            = lv_stlal_lot
              iv_active_bom_01      = 'X'
*             IV_USAGE_PROBABILITY  = 'X'
            CHANGING
              it_stpo               = lt_stpo_bom.
        ELSE.
          CALL METHOD zcl_bom_explode_util=>bom_explode
            EXPORTING
              iv_matnr              = <fs_ebrlotinfo>-output_part
              iv_werks              = '4900'
*             ls_stpo               =
              iv_get_marterial_type = 'X'
*             iv_bom_alt            = lv_stlal_lot
              iv_active_bom_01      = 'X'
*             IV_USAGE_PROBABILITY  = 'X'
            CHANGING
              it_stpo               = lt_stpo_bom.
        ENDIF.
        IF lt_stpo_bom IS NOT INITIAL  .

          DATA(lt_stpo_bom_asy)  =  lt_stpo_bom  .
          DELETE lt_stpo_bom_asy  WHERE mtart  NE 'ZASY'  .
          DATA(lt_stpo_bom_bmp)  =  lt_stpo_bom  .
          DELETE  lt_stpo_bom_bmp  WHERE mtart NE  'ZBMP'  .


           "  delete  lower level sort materials  in BOM  when multi level sort is available in BOM
          " identify  multi level sort with  material group   with highesh    sort material group '133'  or
          "  higher material   and  lower  level material is sort

          LOOP AT lt_stpo_bom  INTO  DATA(ls_stpo_mult_srt)  .
            DATA(lv_tabix)  = sy-tabix .
            IF ls_stpo_mult_srt-higher_mtart  =  'ZSRT'    AND ls_stpo_mult_srt-mtart  = 'ZSRT'  .
              DELETE   lt_stpo_bom  index  lv_tabix.
            ENDIF.
            CLEAR  : ls_stpo_mult_srt ,
                     lv_tabix.
          ENDLOOP.




        ENDIF .

        " get route details and vendor

        " get route vendors  bump  route may not prd INI NPR  fetch bump vendors seperately

        " get route vendors

        SELECT a~matnr
               a~werks
               a~plnty
               a~plnnr
               a~plnal
               b~zaehl
               b~datuv
               b~statu
               b~plnnr_alt  INTO TABLE lt_mapl  FROM mapl   AS a  INNER JOIN plko  AS b ON    a~plnty  =   b~plnty
                                                                                         AND   a~plnnr  =   b~plnnr
                                                                                         AND   a~plnal  =   b~plnal
                                                                                    FOR ALL ENTRIES IN lt_stpo_bom "#EC CI_NO_TRANSFORM
                                                                                     WHERE a~matnr  = lt_stpo_bom-matnr
                                                                                      AND  a~plnty  = 'N'
                                                                                      AND  b~verwe   NE 'CST' "IN ('PRD','INI','NPR')
                                                                                      AND  a~loekz   EQ  ' ' .
*                                                                                      AND  b~statu   NE  '5'.
        " gte assembly routes
        IF lt_stpo_bom_asy IS NOT INITIAL .
          " get  mapl
          SELECT  a~matnr
                  a~werks
                  a~plnty
                  a~plnnr
                  a~plnal
                  b~zaehl
                  b~datuv
                  b~statu
                  b~plnnr_alt   APPENDING TABLE lt_mapl  FROM mapl   AS a  INNER JOIN plko  AS b ON    a~plnty  =   b~plnty
                                                                                    AND   a~plnnr  =   b~plnnr
                                                                                    AND   a~plnal  =   b~plnal
                                                                               FOR ALL ENTRIES IN lt_stpo_bom_asy "#EC CI_NO_TRANSFORM
                                                                                WHERE a~matnr  = lt_stpo_bom_asy-matnr
                                                                                 AND  a~werks  =  '4900'
                                                                                 AND  a~plnty  =  'N'
                                                                                 AND  b~verwe  NE 'CST'
                                                                                 AND  a~loekz  EQ  ' ' .
*                                                                                 AND  b~statu   NE  '5'.
        ENDIF.
        " get bump routes
        IF lt_stpo_bom_bmp IS NOT INITIAL .
          " get bump routes
          SELECT  a~matnr
                  a~werks
                  a~plnty
                  a~plnnr
                  a~plnal
                  b~zaehl
                  b~datuv
                  b~statu
                  b~plnnr_alt   APPENDING TABLE lt_mapl  FROM mapl   AS a  INNER JOIN plko  AS b ON    a~plnty  =   b~plnty
                                                                                    AND   a~plnnr  =   b~plnnr
                                                                                    AND   a~plnal  =   b~plnal
                                                                               FOR ALL ENTRIES IN lt_stpo_bom_bmp
                                                                                WHERE a~matnr   = lt_stpo_bom_bmp-matnr
                                                                                 AND  a~werks   =  '4900'
                                                                                 AND  a~plnty   =  'N'
                                                                                 AND  b~verwe   NE 'CST'
                                                                                 AND  a~loekz   EQ ' ' .
*                                                                                 AND  b~statu   NE  '5'.
        ENDIF.
        IF lt_mapl IS NOT INITIAL.
          SORT lt_mapl  BY plnnr  plnal ASCENDING  datuv  DESCENDING .
          DELETE ADJACENT DUPLICATES FROM  lt_mapl  COMPARING plnnr plnal  .
          DELETE lt_mapl WHERE statu  EQ  '5' .
          SELECT werks , name1  FROM t001w INTO TABLE @DATA(lt_t001w) FOR ALL ENTRIES IN @lt_mapl WHERE
                            werks  =  @lt_mapl-werks  .
          IF sy-subrc EQ 0.
            SORT lt_t001w   BY werks  .
          ENDIF.
          SELECT a~plnty                           "#EC CI_NO_TRANSFORM
                 a~plnnr
                 a~plnal
                 a~plnkn
                 a~zaehl
                 b~datuv
                 b~loekz
                 b~vornr
                 b~werks
                 b~lifnr
                 b~infnr   INTO TABLE lt_plas_plpo  FROM plas AS a  INNER JOIN plpo AS b
                                                  ON    a~plnty   =   b~plnty
                                                 AND    a~plnnr   =   b~plnnr
                                                 AND    a~plnkn   =   b~plnkn
                                       FOR ALL ENTRIES IN  lt_mapl
                                               WHERE  a~plnty  =  lt_mapl-plnty
                                                AND   a~plnnr  =  lt_mapl-plnnr
                                                AND   a~plnal  =  lt_mapl-plnal
                                                AND   b~werks  =  lt_mapl-werks .
          IF sy-subrc EQ 0.
            SORT lt_plas_plpo  BY plnty plnnr plnal ASCENDING datuv  DESCENDING  .
            DELETE ADJACENT DUPLICATES FROM lt_plas_plpo   COMPARING plnty plnnr plnal vornr .
            DELETE lt_plas_plpo WHERE loekz  =  'X'  .

            SELECT infnr , matnr  ,lifnr FROM  eina INTO TABLE  @DATA(lt_eina)   FOR ALL ENTRIES IN  @lt_plas_plpo  WHERE infnr  = @lt_plas_plpo-infnr .
            IF sy-subrc EQ 0.
              SORT lt_eina  BY infnr .
              LOOP AT lt_plas_plpo  ASSIGNING FIELD-SYMBOL(<fs_plas_plpo_upd>)  WHERE infnr IS NOT INITIAL  .
                READ TABLE  lt_eina INTO  DATA(ls_eina_plpo)  WITH KEY  infnr  =  <fs_plas_plpo_upd>-infnr .
                IF sy-subrc EQ 0.
                  <fs_plas_plpo_upd>-lifnr   =  ls_eina_plpo-lifnr .
                ENDIF.
              ENDLOOP .
            ENDIF.

          ENDIF.

          " build route vendors
          LOOP AT lt_stpo_bom INTO  DATA(ls_stpo_route)  .
            LOOP AT lt_mapl  INTO DATA(ls_mapl)  WHERE matnr  = ls_stpo_route-matnr.
              IF ls_stpo_route-mtart  = 'ZASY'  OR ls_stpo_route-mtart  = 'ZBMP' .
                IF ls_mapl-werks    =  '4900' .
                  " get the rout of last operation .
                  DATA(lt_plas_plpo_tmp)  =  lt_plas_plpo .
                  DELETE lt_plas_plpo_tmp WHERE plnnr NE ls_mapl-plnnr  .
                  DELETE lt_plas_plpo_tmp WHERE plnal NE ls_mapl-plnal .
                  SORT  lt_plas_plpo_tmp  BY vornr  DESCENDING .        "  to pick route other than die coat operation which will be at first always
                  READ TABLE lt_plas_plpo_tmp  INTO DATA(ls_plas_plpo_tmp)  INDEX 1 .
                  IF sy-subrc EQ 0.
                    ls_stpo_route-lifnr  =  ls_plas_plpo_tmp-lifnr .
                    APPEND ls_stpo_route  TO lt_stpo_route .
                  ENDIF .
                  CLEAR : lt_plas_plpo_tmp[] ,
                          ls_plas_plpo_tmp  .
                ENDIF.
              ELSE.
                " get vendor in any opertation where vendor is avialble
                " if vendor is not available put  plant as vendor  as it represnts internal
                lt_plas_plpo_tmp  =  lt_plas_plpo .
                IF lr_deep_entity-mpw_wafer  IS INITIAL  .
                  IF ls_stpo_route-mtart  = 'ZSRT'  .
                    DATA(lv_plnnr)  =  <fs_ebrlotinfo>-sort_facility+0(8) .
                    DATA(lv_plnal)  =  <fs_ebrlotinfo>-sort_facility+8(2) .
                    <fs_ebrlotinfo>-sort_plnnr  =   lv_plnnr .
                    <fs_ebrlotinfo>-sort_plnal  =   lv_plnal .
                    lv_srt_plnnr                =   lv_plnnr .
                    lv_srt_plnal                =   lv_plnal .
                    READ TABLE lt_mapl   INTO  DATA(ls_mapl_old)  WITH KEY plnnr  =  lv_plnnr
                                                                           plnal  = lv_plnal .
                    IF sy-subrc EQ 0.
                      lv_srt_plnnr_alt   = ls_mapl_old-plnnr_alt .
                      <fs_ebrlotinfo>-sort_plnnr_alt   =  ls_mapl_old-plnnr_alt .
                    ENDIF.

                  ELSEIF ls_stpo_route-mtart  = 'ZPBS'  .
                    lv_plnnr  =  <fs_ebrlotinfo>-pbs_facility+0(8) .
                    lv_plnal  =  <fs_ebrlotinfo>-pbs_facility+8(2) .
                    <fs_ebrlotinfo>-pbs_plnnr  =   lv_plnnr .
                    <fs_ebrlotinfo>-pbs_plnal  =   lv_plnal .
                    lv_pbs_plnnr                =   lv_plnnr .
                    lv_pbs_plnal                =   lv_plnal .
                    READ TABLE lt_mapl   INTO  ls_mapl_old  WITH KEY plnnr  =  lv_plnnr
                                                       plnal  = lv_plnal .
                    IF sy-subrc EQ 0.
                      lv_pbs_plnnr_alt  = ls_mapl_old-plnnr_alt .
                      <fs_ebrlotinfo>-pbs_plnnr_alt   =  ls_mapl_old-plnnr_alt .
                    ENDIF.

                  ELSEIF ls_stpo_route-mtart  = 'ZTST'  .
                    lv_plnnr  =  <fs_ebrlotinfo>-final_tst_facility+0(8) .
                    lv_plnal  =  <fs_ebrlotinfo>-final_tst_facility+8(2) .
                    <fs_ebrlotinfo>-final_tst_plnnr  =   lv_plnnr .
                    <fs_ebrlotinfo>-final_tst_plnal  =   lv_plnal .
                    lv_tst_plnnr                =   lv_plnnr .
                    lv_tst_plnal                =   lv_plnal .
                    READ TABLE lt_mapl   INTO  ls_mapl_old  WITH KEY plnnr  =  lv_plnnr
                                                       plnal  = lv_plnal .
                    IF sy-subrc EQ 0.
                      lv_tst_plnnr_alt                      =  ls_mapl_old-plnnr_alt .
                      <fs_ebrlotinfo>-final_tst_plnnr_alt   =  ls_mapl_old-plnnr_alt .

                    ENDIF.
                  ELSEIF  ls_stpo_route-mtart  = 'FERT'  .
                    lv_plnnr  =  <fs_ebrlotinfo>-finish_facility+0(8) .
                    lv_plnal  =  <fs_ebrlotinfo>-finish_facility+8(2) .
                    <fs_ebrlotinfo>-finish_plnnr  =   lv_plnnr .
                    <fs_ebrlotinfo>-finish_plnal  =   lv_plnal .
                    lv_fert_plnnr                =   lv_plnnr .
                    lv_fert_plnal                =   lv_plnal .
                    READ TABLE lt_mapl   INTO  ls_mapl_old  WITH KEY plnnr  =  lv_plnnr
                                                       plnal  = lv_plnal .
                    IF sy-subrc EQ 0.
                      lv_fert_plnnr_alt                  = ls_mapl_old-plnnr_alt .
                      <fs_ebrlotinfo>-finish_plnnr_alt   =  ls_mapl_old-plnnr_alt .
                    ENDIF.
                  ENDIF.
                ELSE.
                  lv_plnnr  = ls_mapl-plnnr .
                  lv_plnal  = ls_mapl-plnal .
                  IF ls_stpo_route-mtart  = 'ZSRT'  .
                    <fs_ebrlotinfo>-sort_plnnr  =   lv_plnnr .
                    <fs_ebrlotinfo>-sort_plnal  =   lv_plnal .
                    lv_srt_plnnr                =   lv_plnnr .
                    lv_srt_plnal                =   lv_plnal .
                    lv_srt_plnnr_alt            =   ls_mapl-plnnr_alt .


                  ELSEIF ls_stpo_route-mtart    = 'ZPBS'  .
                    <fs_ebrlotinfo>-pbs_plnnr   =   lv_plnnr .
                    <fs_ebrlotinfo>-pbs_plnal   =   lv_plnal .
                    lv_pbs_plnnr                =   lv_plnnr .
                    lv_pbs_plnal                =   lv_plnal .
                    lv_pbs_plnnr_alt            =   ls_mapl-plnnr_alt .


                  ELSEIF ls_stpo_route-mtart         = 'ZTST'  .
                    <fs_ebrlotinfo>-final_tst_plnnr  =   lv_plnnr .
                    <fs_ebrlotinfo>-final_tst_plnal  =   lv_plnal .
                    lv_tst_plnnr                     =   lv_plnnr .
                    lv_tst_plnal                     =   lv_plnal .
                    lv_tst_plnnr_alt                 =   ls_mapl-plnnr_alt .
                  ELSEIF  ls_stpo_route-mtart  = 'FERT'  .
                    <fs_ebrlotinfo>-finish_plnnr  =   lv_plnnr .
                    <fs_ebrlotinfo>-finish_plnal  =   lv_plnal .
                    lv_fert_plnnr                 =   lv_plnnr .
                    lv_fert_plnal                 =   lv_plnal .
                    lv_fert_plnnr_alt               = ls_mapl-plnnr_alt .
                  ENDIF.
                ENDIF.

              ENDIF.

              DELETE lt_plas_plpo_tmp WHERE plnnr NE lv_plnnr  .
              DELETE lt_plas_plpo_tmp WHERE plnal NE lv_plnal .
*                DELETE lt_plas_plpo_tmp WHERE lifnr  EQ space .
              READ TABLE lt_plas_plpo_tmp  INTO ls_plas_plpo_tmp    INDEX 1 .
              IF sy-subrc EQ 0 AND ls_plas_plpo_tmp-lifnr IS NOT INITIAL .
                ls_stpo_route-lifnr  =   ls_plas_plpo_tmp-lifnr  .
                APPEND ls_stpo_route  TO lt_stpo_route .
              ELSEIF sy-subrc  EQ 0  AND ls_plas_plpo_tmp-infnr IS  NOT INITIAL.
                READ TABLE lt_eina INTO DATA(ls_eina)  WITH KEY infnr  = ls_plas_plpo_tmp-infnr  BINARY SEARCH .
                IF sy-subrc EQ 0.
                  ls_stpo_route-lifnr   = ls_eina-lifnr .
                ENDIF.
                APPEND ls_stpo_route  TO lt_stpo_route .
              ELSEIF ls_plas_plpo_tmp  IS NOT INITIAL.
                " send mapl plant  only when route plant are same
                IF ls_plas_plpo_tmp IS NOT INITIAL    AND  ls_plas_plpo_tmp-werks  EQ ls_mapl-werks .
                  ls_stpo_route-lifnr   = ls_mapl-werks .
                  READ TABLE  lt_t001w  INTO DATA(ls_t001w)  WITH KEY werks  = ls_mapl-werks .
                  IF sy-subrc EQ 0 .
                    ls_stpo_route-lifnr_desc   =  ls_t001w-name1  .
                  ENDIF.
                  APPEND ls_stpo_route  TO lt_stpo_route .
                ENDIF.
              ENDIF.
              IF ls_stpo_route-lifnr IS NOT INITIAL .
                IF ls_stpo_route-mtart          = 'ZSRT'  .
                  lv_sort_facility              =      ls_stpo_route-lifnr .
                ELSEIF ls_stpo_route-mtart      = 'ZPBS'  .
                  lv_pbs_facility               =      ls_stpo_route-lifnr .
                ELSEIF ls_stpo_route-mtart      = 'ZTST'  .
                  lv_final_tst_facility         =      ls_stpo_route-lifnr .
                ELSEIF  ls_stpo_route-mtart     = 'FERT'  .
                  lv_finish_facility            =     ls_stpo_route-lifnr .
                ENDIF.
              ELSE.
                IF ls_stpo_route-mtart      = 'ZPBS'  .
                  READ TABLE lt_zebr_config_dc  INTO DATA(ls_ebr_config_dc) WITH KEY  field1  =  <fs_ebrlotinfo>-finish_facility  .
                  IF sy-subrc EQ 0.
                    lv_pbs_facility               =    ls_ebr_config_dc-field1 .
                  ENDIF.
                ELSEIF ls_stpo_route-mtart      = 'ZTST'  .
                  READ TABLE lt_zebr_config_dc  INTO ls_ebr_config_dc WITH KEY  field1  =  <fs_ebrlotinfo>-final_tst_facility  .
                  IF sy-subrc EQ 0.
                    lv_final_tst_facility               =    ls_ebr_config_dc-field1 .
                  ENDIF.
                ELSEIF  ls_stpo_route-mtart     = 'FERT'  .
                  READ TABLE lt_zebr_config_dc  INTO ls_ebr_config_dc WITH KEY  field1  =  <fs_ebrlotinfo>-finish_facility  .
                  IF sy-subrc EQ 0.
                    lv_finish_facility               =    ls_ebr_config_dc-field1 .
                  ENDIF.
                ENDIF.
              ENDIF.
              CLEAR : lt_plas_plpo_tmp[] ,
                      ls_plas_plpo_tmp  .
            ENDLOOP .
            CLEAR  ls_stpo_route .
          ENDLOOP.
          APPEND LINES OF lt_stpo_route  TO lt_stpo_bom .
        ENDIF.
        CLEAR  : ls_mkal_lot ,
                 lt_plas_plpo_mpw_lot[] ,
                 lt_mapl_fert_lot[],
                 lv_fert_plnal_det_lot,
                 lv_fert_plnnr_det_lot.

      ENDAT .
      IF lr_deep_entity-mpw_wafer  IS INITIAL  .
        <fs_ebrlotinfo>-sort_facility       = lv_sort_facility .
        <fs_ebrlotinfo>-pbs_facility        = lv_pbs_facility .
        <fs_ebrlotinfo>-final_tst_facility  = lv_final_tst_facility .
        <fs_ebrlotinfo>-finish_facility     = lv_finish_facility.
        <fs_ebrlotinfo>-sort_plnnr          =  lv_srt_plnnr .
        <fs_ebrlotinfo>-sort_plnal          =  lv_srt_plnal .
        <fs_ebrlotinfo>-pbs_plnnr           =  lv_pbs_plnnr .
        <fs_ebrlotinfo>-pbs_plnal           =  lv_pbs_plnal .
        <fs_ebrlotinfo>-final_tst_plnnr     =  lv_tst_plnnr .
        <fs_ebrlotinfo>-final_tst_plnal     =  lv_tst_plnal .
        <fs_ebrlotinfo>-finish_plnnr        =  lv_fert_plnnr .
        <fs_ebrlotinfo>-finish_plnal        =  lv_fert_plnal .
        <fs_ebrlotinfo>-sort_plnnr_alt      =  lv_srt_plnnr_alt .
        <fs_ebrlotinfo>-pbs_plnnr_alt       =  lv_pbs_plnnr_alt .
        <fs_ebrlotinfo>-final_tst_plnnr_alt =  lv_tst_plnnr_alt .
        <fs_ebrlotinfo>-finish_plnnr_alt    =  lv_fert_plnnr_alt .

      ENDIF.
      lv_waferqty    = <fs_ebrlotinfo>-wafer_qty  .
      IF <fs_ebrlotinfo>-lot_no  IS NOT INITIAL   AND <fs_ebrlotinfo>-wafer_no  IS INITIAL .

        DATA    lo_wf_exception  TYPE REF TO  /iwbep/cx_mgw_tech_exception   .
        CREATE OBJECT lo_wf_exception .
        IF lo_wf_exception IS  BOUND .
          lo_wf_exception->get_msg_container( )->add_message(
                   EXPORTING
                     iv_msg_type     = 'E'
                     iv_msg_id      = '00'
                     iv_msg_number  = '123'
                     iv_msg_text    = 'Select Wafer No  as any for lot  :' && <fs_ebrlotinfo>-lot_no ).
          RAISE EXCEPTION lo_wf_exception .
          RETURN .
        ENDIF.
      ELSEIF <fs_ebrlotinfo>-lot_no  IS NOT INITIAL   AND lv_waferqty  GT '25' .
        DATA    lo_wf_exception_25  TYPE REF TO  /iwbep/cx_mgw_tech_exception   .
        CREATE OBJECT lo_wf_exception_25 .
        IF lo_wf_exception_25 IS  BOUND .
          lo_wf_exception_25->get_msg_container( )->add_message(
                   EXPORTING
                     iv_msg_type    = 'E'
                     iv_msg_id      = '00'
                     iv_msg_number  = '123'
                     iv_msg_text    = 'Wafer QTY is greater than 25  :' && <fs_ebrlotinfo>-lot_no ).
          RAISE EXCEPTION lo_wf_exception_25 .
          RETURN .
        ENDIF.
      ELSEIF <fs_ebrlotinfo>-lot_no  IS NOT INITIAL   AND lv_waferqty  LT '3' .
        CLEAR  lv_product_type .
        CALL METHOD zcl_zebr_form_dpc_ext=>get_fert_characteristics
          EXPORTING
            iv_matnr        = <fs_ebrlotinfo>-output_part
          IMPORTING
            ev_product_type = lv_product_type.
        IF lv_product_type  EQ '03'  .
          " show error when  product type is cs/wlp  i.e,  product type 03   and when quantity is less tha 03
          lr_deep_entity-inform_message   =  'Need approval for less than 3wfrs loading from Michael Lu ' && <fs_ebrlotinfo>-lot_no  .
          IF lv_req_status_no  = '00'  AND lv_target_status_no  =  '01' .
            SELECT  SINGLE  req_no  FROM zebr_t_file  INTO @DATA(ls_zebr_t_file)  WHERE req_no  =  @lr_deep_entity-req_no .
            IF sy-subrc NE  0 .
              " send mail
              TRY.
                  CALL METHOD zcl_ebr_utility=>email_functionality
                    EXPORTING
                      im_ebr        = lv_reqid
                      im_req_type   = 'A'
                      im_req_status = lr_deep_entity-req_status_no
                      im_group      = zcl_ebr_utility=>zemail_grp_sub_wlp
*            IMPORTING
*                     et_notif      =
                    .
                CATCH cx_abap_invalid_value .
              ENDTRY.
              DATA    lo_wf_exception_3  TYPE REF TO  /iwbep/cx_mgw_tech_exception   .
              CREATE OBJECT lo_wf_exception_3 .
              IF lo_wf_exception_3 IS  BOUND .
                lo_wf_exception_3->get_msg_container( )->add_message(
                         EXPORTING
                           iv_msg_type    = 'E'
                           iv_msg_id      = '00'
                           iv_msg_number  = '123'
                           iv_msg_text    = 'Need approval for less than 3wfrs loading from Michael Lu,Please Attach approval' ).
                RAISE EXCEPTION lo_wf_exception_3 .
                RETURN .
              ENDIF.

            ENDIF.

          ENDIF.
        ENDIF.

      ENDIF.

*TRY.
      " prepare master data
      CALL METHOD zcl_zebr_form_dpc_ext=>fill_masterdata
        EXPORTING
          iv_out_mtart     = lv_out_mtart
          iv_reqid         = lv_reqid
          iv_lot_matnr     = lv_lot_matnr
        CHANGING
          ct_stpo_bom      = lt_stpo_bom
          ct_master_status = lt_master_status
          cr_deep_entity   = lr_deep_entity
          ct_mapl          = lt_mapl
          ct_plas_plpo     = lt_plas_plpo
          cs_ebrlotinfo    = <fs_ebrlotinfo>
          ct_mch1          = lt_mch1
          crt_mtart        = lrt_mtart.
* CATCH /iwbep/cx_mgw_busi_exception .
* CATCH /iwbep/cx_mgw_tech_exception .
*ENDTRY.


*      ENDIF.

      "  die  quantity  = wfr quantity  *  gdpw
      IF <fs_ebrlotinfo>-wafer_qty  IS NOT  INITIAL    .
        READ TABLE  lt_mch1 INTO DATA(ls_mch1_gdpw)   WITH KEY licha  = <fs_ebrlotinfo>-lot_no
                                                               mtart  = 'ZSRT'.
        IF sy-subrc EQ 0  .
          "  zfab material
          CALL METHOD zcl_bom_explode_util=>bom_explode
            EXPORTING
              iv_matnr              = ls_mch1_gdpw-matnr
              iv_werks              = '4900'
*             ls_stpo               =
              iv_get_marterial_type = 'X'
**             iv_active_bom         =
*             iv_bom_alt            = ls_stpo_bom-alt
              iv_active_bom_01      = 'X'
*             IV_USAGE_PROBABILITY  = 'X'
            CHANGING
              it_stpo               = lt_stpo_bom_fab.
          READ TABLE  lt_stpo_bom_fab  INTO  DATA(ls_stpo_bom_fab) WITH KEY  mtart  = 'ZFAB'  .
          IF sy-subrc EQ 0.
            " get fab materil charactertics
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = ls_stpo_bom_fab-matnr
              IMPORTING
                output       = ls_stpo_bom_fab-matnr
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ENDIF.

            CALL METHOD zcl_zebr_form_dpc_ext=>get_mat_characteristics
              EXPORTING
                iv_matnr           = ls_stpo_bom_fab-matnr
              IMPORTING
                et_allocvaluesnum  = lt_allocvaluesnum
                et_allocvalueschar = lt_allocvalueschar
                et_allocvaluescurr = lt_allocvaluescurr
                et_return          = lt_return.
            READ TABLE lt_allocvaluesnum INTO DATA(ls_num) WITH KEY  charact  = 'GDPW' .
            IF sy-subrc EQ 0.
              IF  <fs_ebrlotinfo>-lot_start_qty IS  INITIAL  .
                <fs_ebrlotinfo>-lot_start_qty  = ls_num-value_from  *   <fs_ebrlotinfo>-wafer_qty   .
              ENDIF.
              lr_deep_entity-gdpw            = ls_num-value_from .
              CLEAR ls_num .
            ENDIF.
          ENDIF.

        ELSE .
          " zfab  lot  material
          READ TABLE  lt_mch1 INTO ls_mch1_gdpw   WITH KEY licha  = <fs_ebrlotinfo>-lot_no
                                                              mtart  = 'ZFAB'.
          IF sy-subrc  EQ  0.
            CALL METHOD zcl_zebr_form_dpc_ext=>get_mat_characteristics
              EXPORTING
                iv_matnr           = ls_mch1_gdpw-matnr
              IMPORTING
                et_allocvaluesnum  = lt_allocvaluesnum
                et_allocvalueschar = lt_allocvalueschar
                et_allocvaluescurr = lt_allocvaluescurr
                et_return          = lt_return.
            READ TABLE lt_allocvaluesnum INTO ls_num WITH KEY  charact  = 'GDPW' .
            IF sy-subrc EQ 0.
              IF <fs_ebrlotinfo>-lot_start_qty IS  INITIAL  .
                <fs_ebrlotinfo>-lot_start_qty  = ls_num-value_from  *   <fs_ebrlotinfo>-wafer_qty   .
              ENDIF.
              lr_deep_entity-gdpw            = ls_num-value_from .
              CLEAR ls_num .
            ENDIF.

          ENDIF.

        ENDIF .
      ENDIF.

      " rel id from sort /bacth characteristics

      CLEAR lv_mfrpn  .

      APPEND INITIAL LINE TO lt_lotinfo[]  ASSIGNING FIELD-SYMBOL(<fs_lotinfo>)  .
      MOVE-CORRESPONDING   <fs_ebrlotinfo>  TO <fs_lotinfo>.
      CLEAR  lv_waferqty .
    ENDLOOP .

    "below data  is used  to find whether lot is zsrt  or zfab
    " material type is used in  validate master data method to conisder or skip zsrt materils status  to calculate final status.
    SELECT a~matnr , a~charg  , a~licha , b~werks ,b~lgort , b~clabs ,b~cspem , c~mtart ,c~mfrpn
                            FROM  mch1  AS a INNER JOIN  mchb  AS b  ON a~matnr  = b~matnr     AND a~charg   =  b~charg
                                                                         INNER JOIN  mara AS c
                                                                                 ON b~matnr  =  c~matnr
                                                                         INNER JOIN t320 AS d
                                                                                 ON  b~werks  =  d~werks
                                                                                 AND   b~lgort  =  d~lgort
                                                                    INTO TABLE @DATA(lt_mch1_mtart)
                                                                  FOR ALL ENTRIES IN  @lr_deep_entity-ebrheadlotnav
                                                                       WHERE a~licha EQ @lr_deep_entity-ebrheadlotnav-lot_no
                                                                        AND b~clabs GT 0.
    IF sy-subrc  EQ  0 .
      SORT lt_mch1_mtart  BY matnr  .
      READ TABLE  lt_mch1_mtart  INTO DATA(ls_mch1_mtart)  INDEX 1 .
      IF sy-subrc EQ 0.
        DATA(lv_mtart)   =  ls_mch1_mtart-mtart .
        " rel id from sort /bacth characteristics
        " owner code
        IF lr_deep_entity-req_type = zcl_ebr_utility=>zreq_type_auto_qual  OR   lr_deep_entity-req_type = zcl_ebr_utility=>zreq_type_pkg_qual
         OR  lr_deep_entity-req_type = zcl_ebr_utility=>zreq_type_eng_proto   OR  lr_deep_entity-req_type =  zcl_ebr_utility=>zreq_type_si_ver
          OR lr_deep_entity-req_type = zcl_ebr_utility=>zreq_type_fab_qual.
          CALL METHOD zcl_zebr_form_dpc_ext=>get_batch_char
            EXPORTING
              iv_matnr           = ls_mch1_mtart-matnr
              iv_charg           = ls_mch1_mtart-charg
            IMPORTING
              et_allocvaluesnum  = DATA(lt_allocvaluesnum_rel)
              et_allocvalueschar = DATA(lt_allocvalueschar_rel)
              et_allocvaluescurr = DATA(lt_allocvaluescurr_rel)
              et_return          = DATA(lt_return_t_rel).
          READ TABLE lt_allocvalueschar_rel INTO DATA(ls_char) WITH KEY  charact  = 'RELID' .
          IF sy-subrc EQ 0.
            ls_header-relid  = ls_char-value_char  .
            lr_deep_entity-relid  = ls_char-value_char  .
            IF lr_deep_entity-relid IS NOT INITIAL .
              ls_header-req_rel   = 'X'  .
              lr_deep_entity-req_rel  = 'X'  .
            ENDIF.
            CLEAR ls_char .
          ENDIF.


          REFRESH  : lt_allocvaluesnum_rel[] ,
                     lt_allocvalueschar_rel[] ,
                     lt_allocvaluescurr_rel[] ,
                     lt_return_t_rel[].
        ENDIF.

      ENDIF.

    ENDIF.



    LOOP AT  lr_deep_entity-ebrnotifnav  ASSIGNING FIELD-SYMBOL(<fs_ebrnotification>)  .
      <fs_ebrnotification>-req_no  = lv_reqid .
      IF <fs_ebrnotification>-email IS NOT INITIAL .
        APPEND INITIAL LINE TO lt_notif[]  ASSIGNING FIELD-SYMBOL(<fs_notification>)  .
        MOVE-CORRESPONDING   <fs_ebrnotification>  TO <fs_notification>.
      ENDIF.
    ENDLOOP .


    IF lr_deep_entity-lot_induct  EQ  'X'  .
      lr_deep_entity-supply_status  = 'G' .
      ls_header-supply_status   =  lr_deep_entity-supply_status .
    ENDIF.
    " validate  modify master data status
    CALL METHOD zcl_ebr_utility=>validate_modify_master_data
      EXPORTING
        ev_mtart             = lv_mtart
      IMPORTING
        ev_masterdata_status = lr_deep_entity-masterdata_status
      CHANGING
        ct_master_data       = lt_master_status.

    IF lr_deep_entity-issue_pdm_team  IS INITIAL AND lr_deep_entity-issue_assy_team   IS INITIAL .
      ls_header-masterdata_status    = lr_deep_entity-masterdata_status  .
    ELSE.
      lr_deep_entity-masterdata_status  =  'R'  .
      ls_header-masterdata_status    = lr_deep_entity-masterdata_status  .
    ENDIF.

    IF lr_deep_entity-ebr_validate IS NOT INITIAL  OR  lr_deep_entity-target_status_no  = '03' .


      TRY .
          CALL METHOD zcl_zebr_form_dpc_ext=>validate_ebr
            EXPORTING
              is_ebrheader = lr_deep_entity.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception   INTO lo_exception .
          IF lo_exception IS BOUND .
            RAISE EXCEPTION lo_exception .
            RETURN.
          ENDIF.
      ENDTRY .
    ENDIF.

    IF lr_deep_entity-ebr_validate IS INITIAL .
      " generate req ID/ EBR NUMBER  .
      IF lr_deep_entity-req_no EQ '*'  .
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = '01'
            object                  = 'ZEBR_NR'
*           QUANTITY                = '1'
*           SUBOBJECT               = ' '
*           TOYEAR                  = '0000'
            ignore_buffer           = 'X'
          IMPORTING
            number                  = lv_reqid1
*           QUANTITY                =
*           RETURNCODE              =
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.
        IF sy-subrc <> 0.
          CREATE OBJECT lo_exception.
          lo_exception->get_msg_container( )->add_message(
             EXPORTING
               iv_msg_type     = 'E'
               iv_msg_id      = '00'
               iv_msg_number  = '123'
               iv_msg_text    = 'Failed to draw number for Request Id' ).
          RAISE EXCEPTION lo_exception.
          RETURN.
        ENDIF.

        WRITE lv_reqid1 TO lv_reqid NO-ZERO.
        CONDENSE lv_reqid.
        " get requestor  and group
        SELECT SINGLE bname  accnt   FROM usr02  INTO ls_usr  WHERE bname = sy-uname .
        IF ls_usr IS NOT INITIAL .
          SELECT  SINGLE employee_id displayname maximorg  FROM zworkday_data  INTO ls_workday WHERE  employee_id = ls_usr-accnt .
        ENDIF .

        lr_deep_entity-req_no        = lv_reqid .
        ls_header-created_by         = sy-uname.
        ls_header-created_on         = sy-datum .
        ls_header-created_time       = sy-uzeit .
        ls_header-changed_by         = sy-uname.
        ls_header-changed_on         = sy-datum .
        ls_header-changed_time       = sy-uzeit .
        lr_deep_entity-created_by    = ls_workday-displayname  .
        lr_deep_entity-created_on    = sy-datum.
        lr_deep_entity-created_time  = sy-uzeit .
        lr_deep_entity-changed_by    = ls_workday-displayname  .
        lr_deep_entity-changed_on    = sy-datum .
        lr_deep_entity-changed_time  = sy-uzeit .


        APPEND INITIAL LINE TO  lt_stat_log ASSIGNING <fs_stat>  .
        <fs_stat>-req_no         = lv_reqid .
        <fs_stat>-req_status_no  = lr_deep_entity-req_status_no .
        <fs_stat>-req_status     = lr_deep_entity-req_status  .
        <fs_stat>-created_by     = sy-uname .
        <fs_stat>-created_at     = sy-uzeit.
        <fs_stat>-created_on     = sy-datum .

        " below staus update is when user click on directly submit instead of save when EBR number is not yet generated
        IF lr_deep_entity-req_status_no  NE lr_deep_entity-target_status_no  AND lr_deep_entity-target_status_no = '01' .
          CLEAR :  lv_req_status_no  ,
                   lv_target_status_no .
          APPEND INITIAL LINE TO  lt_stat_log ASSIGNING <fs_stat> .
          <fs_stat>-req_no              = lv_reqid .
          <fs_stat>-req_status_no       = lr_deep_entity-target_status_no .
          <fs_stat>-req_status          = lr_deep_entity-target_status  .
          <fs_stat>-created_by          = sy-uname .
          <fs_stat>-created_at          = sy-uzeit .
          <fs_stat>-created_on          = sy-datum .
          lv_req_status_no              = lr_deep_entity-req_status_no  .
          lv_target_status_no           = lr_deep_entity-target_status_no .
          lr_deep_entity-req_status_no  = lr_deep_entity-target_status_no .
          lr_deep_entity-req_status     = lr_deep_entity-target_status  .
        ENDIF.

      ENDIF.
      IF ls_header-req_no IS NOT INITIAL .
        SELECT SINGLE  npp_processor  , issue_pdm_team , issue_assy_team   FROM zebr_t_header  INTO @DATA(ls_header_det)  WHERE  req_no  =  @ls_header-req_no  .
        IF sy-subrc  EQ 0  AND ls_header_det-npp_processor  IS NOT INITIAL.
          ls_header-npp_processor   =  ls_header_det-npp_processor  .
        ENDIF.
      ENDIF.
      ls_header-req_no  = lv_reqid .
      "  validate and update master status
      IF lt_master_status IS NOT INITIAL .
        LOOP AT lt_master_status  ASSIGNING FIELD-SYMBOL(<fs_master_status_req_no>) .
          <fs_master_status_req_no>-req_no  =  lv_reqid .
        ENDLOOP .
        CALL METHOD zcl_ebr_utility=>validate_modify_master_data
          EXPORTING
            ev_mtart             = lv_mtart
          IMPORTING
            ev_masterdata_status = lr_deep_entity-masterdata_status
          CHANGING
            ct_master_data       = lt_master_status.

        IF lr_deep_entity-issue_assy_team  IS INITIAL  AND lr_deep_entity-issue_pdm_team  IS INITIAL .
          ls_header-masterdata_status  = lr_deep_entity-masterdata_status .
        ELSE.
          lr_deep_entity-masterdata_status   = 'R'  .
          ls_header-masterdata_status  = lr_deep_entity-masterdata_status .
        ENDIF.
      ENDIF.
      ls_header-copied_ebr = lr_deep_entity-copied_ebr.
      APPEND ls_header TO lt_header .
      CLEAR ls_header  .



      IF lr_deep_entity-ebrheadlotnav[] IS NOT INITIAL .
        "  get planning id for existing records before deleting .
        IF lr_deep_entity-req_status_no  EQ  '98'   OR lv_req_status_no EQ '98' .
          SELECT  * FROM  zebr_t_lotinfo  INTO TABLE @DATA(lt_lotinfo_plan)  WHERE req_no  =  @lr_deep_entity-req_no . "#EC CI_ALL_FIELDS_NEEDED
        ENDIF.
        LOOP AT lt_lotinfo   ASSIGNING FIELD-SYMBOL(<fs_lotinfo_req>)  .
          IF <fs_lotinfo_req>-lot_no   IS  INITIAL .
            DELETE  TABLE lt_lotinfo    FROM  <fs_lotinfo_req> .
          ELSE.
            <fs_lotinfo_req>-req_no  =  lv_reqid .
            " when status 98 planning id are already generated  for same lots and req_no's
            " when status is moving from 98  to 03  planning is already generated .
            IF lr_deep_entity-req_status_no  EQ  '98'   OR lr_deep_entity-target_status_no  EQ  '03' .
              READ TABLE  lt_lotinfo_plan  INTO DATA(ls_lotinfo_plan)  WITH KEY  req_no       = <fs_lotinfo_req>-req_no
                                                                                 output_part  =  <fs_lotinfo_req>-output_part. .
              IF sy-subrc EQ 0 AND ls_lotinfo_plan-planning_id IS NOT INITIAL .
                <fs_lotinfo_req>-planning_id  =  ls_lotinfo_plan-planning_id .
                " validate   whether planning id has  production order or STO's
                " VGUNT32132 ++
                TRY .
                    CALL METHOD zcl_zebr_form_dpc_ext=>validate_planning_id
                      EXPORTING
                        iv_planning_id = <fs_lotinfo_req>-planning_id.
                  CATCH /iwbep/cx_mgw_busi_exception .
                  CATCH /iwbep/cx_mgw_tech_exception   INTO lo_exception .
                    IF lo_exception IS BOUND .
                      RAISE EXCEPTION lo_exception .
                      RETURN.
                    ENDIF.
                ENDTRY .
              ENDIF.
            ENDIF .
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF lr_deep_entity-ebrnotifnav[] IS NOT INITIAL .
        LOOP AT lt_notif ASSIGNING  FIELD-SYMBOL(<fs_notif_req>)  .
          <fs_notif_req>-req_no  = lv_reqid .
        ENDLOOP .
      ENDIF.

      IF lt_stat_log IS NOT INITIAL .
        LOOP AT lt_stat_log ASSIGNING  FIELD-SYMBOL(<fs_stat_log_req>)  .
          <fs_stat_log_req>-req_no  = lv_reqid .
        ENDLOOP .
      ENDIF.
      "  save values to database in update function module
      CALL FUNCTION 'ZEBR_TABLES_UPDATE' IN UPDATE TASK
        TABLES
          it_header   = lt_header
          it_lotinfo  = lt_lotinfo
          it_stat_log = lt_stat_log
          it_notif    = lt_notif.

      COMMIT WORK AND WAIT .

      " update when there is mismatch ebr header table data
      IF lr_deep_entity-target_status_no IS NOT INITIAL .

        DO 10 TIMES.

          SELECT SINGLE  * FROM  zebr_t_header INTO  @DATA(ls_header_check)  WHERE  req_no        =   @lr_deep_entity-req_no
                                                                               AND  req_status_no  =  @lr_deep_entity-target_status_no .
          IF sy-subrc NE 0 .
            MODIFY  zebr_t_header FROM TABLE lt_header .
            IF sy-subrc EQ 0.
              COMMIT WORK .
              WAIT  UP TO 1 SECONDS .
            ENDIF.
          ELSE.
            EXIT .
          ENDIF.
        ENDDO.
      ENDIF.


      "send email
      " submited to npp /send to npp
      IF lv_req_status_no  =  '00'   AND  lv_target_status_no  =  '01'  .
        TRY.
            CALL METHOD zcl_ebr_utility=>email_functionality
              EXPORTING
                im_ebr        = lv_reqid
                im_req_type   = 'N'
                im_req_status = lr_deep_entity-req_status_no
                im_group      = zcl_ebr_utility=>zemail_grp_send_npp
*        IMPORTING
*               et_notif      =
              .
          CATCH cx_abap_invalid_value .
        ENDTRY.
      ENDIF.


      "  issue assembly
      IF lr_deep_entity-issue_assy_team IS NOT INITIAL  AND ls_header_det-issue_assy_team IS INITIAL .
        TRY .
            CALL METHOD zcl_ebr_utility=>email_functionality
              EXPORTING
                im_ebr        = lv_reqid
                im_req_type   = 'N'
                im_req_status = lr_deep_entity-req_status_no
                im_group      = zcl_ebr_utility=>zemail_grp_isu_asy
*        IMPORTING
*               et_notif      = .
              .
          CATCH cx_abap_invalid_value.
        ENDTRY.
      ENDIF.

      " issue  pdm
      IF lr_deep_entity-issue_pdm_team IS NOT INITIAL  AND ls_header_det-issue_pdm_team IS INITIAL .
        TRY .
            CALL METHOD zcl_ebr_utility=>email_functionality
              EXPORTING
                im_ebr        = lv_reqid
                im_req_type   = 'N'
                im_req_status = lr_deep_entity-req_status_no
                im_group      = zcl_ebr_utility=>zemail_grp_isu_pdm
*        IMPORTING
*               et_notif      =
              .
          CATCH cx_abap_invalid_value.
        ENDTRY.
      ENDIF.
    ENDIF.




    CHECK NOT lv_reqid IS INITIAL AND lv_reqid NE '*'.
    lv_name = sy-mandt && lv_reqid.
    IF NOT lr_deep_entity-part_desc IS INITIAL.
      save_text(
        EXPORTING
          id        = zcl_zebr_form_mpc_ext=>gc_tdid_part_des    " Text ID
          name      = lv_name    " Name
          object    = zcl_zebr_form_mpc_ext=>gc_object " Texts: application object
          text      = lr_deep_entity-part_desc ).

    ENDIF.
    IF NOT lr_deep_entity-test_desc IS INITIAL.
      save_text(
         EXPORTING
           id        = zcl_zebr_form_mpc_ext=>gc_tdid_test_des    " Text ID
           name      = lv_name    " Name
           object    = zcl_zebr_form_mpc_ext=>gc_object    " Texts: application object
           text      = lr_deep_entity-test_desc ).
    ENDIF.

    IF NOT lr_deep_entity-assy_instr IS INITIAL.
      save_text(
        EXPORTING
          id        = zcl_zebr_form_mpc_ext=>gc_tdid_assy_instr    " Text ID
          name      = lv_name    " Name
          object    = zcl_zebr_form_mpc_ext=>gc_object " Texts: application object
          text      = lr_deep_entity-assy_instr ).
    ENDIF.
    IF NOT lr_deep_entity-bump_instr IS INITIAL.
      save_text(
         EXPORTING
           id        = zcl_zebr_form_mpc_ext=>gc_tdid_bump_instr   " Text ID
           name      = lv_name    " Name
           object    = zcl_zebr_form_mpc_ext=>gc_object    " Texts: application object
           text      = lr_deep_entity-bump_instr ).
    ENDIF.

    IF NOT lr_deep_entity-pbs_instr IS INITIAL.
      save_text(
        EXPORTING
          id        = zcl_zebr_form_mpc_ext=>gc_tdid_pbs_instr    " Text ID
          name      = lv_name    " Name
          object    = zcl_zebr_form_mpc_ext=>gc_object " Texts: application object
          text      = lr_deep_entity-pbs_instr ).
    ENDIF.
    IF NOT lr_deep_entity-test_instr  IS INITIAL.
      save_text(
         EXPORTING
           id        = zcl_zebr_form_mpc_ext=>gc_tdid_test_instr    " Text ID
           name      = lv_name    " Name
           object    = zcl_zebr_form_mpc_ext=>gc_object    " Texts: application object
           text      = lr_deep_entity-test_instr ).
    ENDIF.

    IF NOT lr_deep_entity-fert_instr IS INITIAL.
      save_text(
         EXPORTING
           id        = zcl_zebr_form_mpc_ext=>gc_tdid_fert_instr    " Text ID
           name      = lv_name    " Name
           object    = zcl_zebr_form_mpc_ext=>gc_object    " Texts: application object
           text      = lr_deep_entity-fert_instr ).
    ENDIF.


    IF NOT lr_deep_entity-add_comment IS INITIAL.
      save_text(
         EXPORTING
           id        = zcl_zebr_form_mpc_ext=>gc_tdid_add_comment   " Text ID
           name      = lv_name    " Name
           object    = zcl_zebr_form_mpc_ext=>gc_object    " Texts: application object
           text      = lr_deep_entity-add_comment ).
    ENDIF.

    IF NOT lr_deep_entity-npp_text  IS INITIAL.
      save_text(
         EXPORTING
           id        = zcl_zebr_form_mpc_ext=>gc_tdid_npp_text    " Text ID
           name      = lv_name    " Name
           object    = zcl_zebr_form_mpc_ext=>gc_object    " Texts: application object
           text      = lr_deep_entity-npp_text ).
    ENDIF.


    IF NOT lr_deep_entity-sort_instr IS INITIAL.
      save_text(
        EXPORTING
          id        = zcl_zebr_form_mpc_ext=>gc_tdid_srt_instr    " Text ID
          name      = lv_name    " Name
          object    = zcl_zebr_form_mpc_ext=>gc_object " Texts: application object
          text      = lr_deep_entity-sort_instr ).
    ENDIF.


    " wafer numbers should be concatenated before  die bank intructions
    " check  if if already  wafer are  available  by checking whether string contains "//"  the characters before  "//" are wafer numbers
    READ TABLE  lt_lotinfo  INTO DATA(ls_lotinfo_wafer)  INDEX 1 .
    IF sy-subrc EQ 0.
      DATA(lv_wafer_no)   = ls_lotinfo_wafer-wafer_no  .
      REPLACE ALL OCCURRENCES OF  '"'   IN  lv_wafer_no WITH space .

      REPLACE all  OCCURRENCES OF ','  in lv_wafer_no  with '-'  .

      CONDENSE lv_wafer_no .
      IF lr_deep_entity-die_instr CS '//'   .
        lr_deep_entity-die_instr   =  lv_wafer_no  && lr_deep_entity-die_instr+sy-fdpos .
      ELSE .
        lr_deep_entity-die_instr   = lv_wafer_no   &&  '//'   && ' ' && lr_deep_entity-die_instr .
      ENDIF.
      CLEAR : lv_wafer_no .
    ENDIF.
    IF NOT lr_deep_entity-die_instr IS INITIAL.
      save_text(
        EXPORTING
          id        = zcl_zebr_form_mpc_ext=>gc_tdid_die_instr    " Text ID
          name      = lv_name    " Name
          object    = zcl_zebr_form_mpc_ext=>gc_object " Texts: application object
          text      = lr_deep_entity-die_instr ).
    ENDIF.

    " update load plan
    IF lo_exception IS BOUND .
      RAISE EXCEPTION lo_exception .
      RETURN.
    ELSE.
      IF lr_deep_entity-target_status_no  = '03'  .
        " create and entry in zscm_load_plan
        TRY .
            CALL METHOD zcl_zebr_form_dpc_ext=>submit_ebr
              EXPORTING
                is_ebrheader     = lr_deep_entity
                it_ebr_mast_data = lt_master_status
              IMPORTING
                ev_planning_id   = lv_planid.
          CATCH /iwbep/cx_mgw_busi_exception .
          CATCH /iwbep/cx_mgw_tech_exception   INTO lo_ebr_sucess.
            IF lo_exception IS BOUND .
              RAISE EXCEPTION lo_ebr_sucess.
              RETURN.
            ENDIF.
        ENDTRY .

      ENDIF.
    ENDIF.
    " set sort finish test pbs  as group + counter since  in UI  application   faiclity are mapped with key as group_counter
    IF lr_deep_entity-mpw_wafer  IS INITIAL  .
      LOOP AT lr_deep_entity-ebrheadlotnav  ASSIGNING FIELD-SYMBOL(<fs_headlotnav_ven>) .
        IF <fs_headlotnav_ven>-sort_facility  IS NOT INITIAL .
          <fs_headlotnav_ven>-sort_facility  =  <fs_headlotnav_ven>-sort_plnnr && <fs_headlotnav_ven>-sort_plnal .
        ENDIF .
        IF <fs_headlotnav_ven>-pbs_facility IS NOT INITIAL .
          <fs_headlotnav_ven>-pbs_facility  =  <fs_headlotnav_ven>-pbs_plnnr && <fs_headlotnav_ven>-pbs_plnal .
        ENDIF.

        IF <fs_headlotnav_ven>-final_tst_facility  IS NOT INITIAL .
          <fs_headlotnav_ven>-final_tst_facility  =  <fs_headlotnav_ven>-final_tst_plnnr && <fs_headlotnav_ven>-final_tst_plnal .

        ENDIF.

        IF <fs_headlotnav_ven>-finish_facility IS NOT INITIAL .
          <fs_headlotnav_ven>-finish_facility  =  <fs_headlotnav_ven>-finish_plnnr && <fs_headlotnav_ven>-finish_plnal .
        ENDIF.
      ENDLOOP.
    ENDIF.
    " send data  to deep entity
    copy_data_to_ref(
         EXPORTING
         is_data = lr_deep_entity
         CHANGING
         cr_data = er_deep_entity ).
    CLEAR  : lv_req_status_no  ,
             lv_target_status_no,
             lv_out_mtart.
  ENDMETHOD.


  METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_STREAM.
    DATA: lw_file TYPE zebr_t_file.
    FIELD-SYMBOLS:<fs_key> TYPE /iwbep/s_mgw_name_value_pair.
*
*    DATA :  lo_facade TYPE REF TO /iwbep/if_mgw_dp_int_facade.
*    lo_facade     ?= /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
*    DATA(lt_client_headers) = lo_facade->get_request_header( ).
    READ TABLE it_key_tab ASSIGNING <fs_key> INDEX 1.
*    DATA(lv_file) = iv_slug .
    SPLIT iv_slug AT '|' INTO  DATA(lv_req_no) DATA(lv_file).
    lw_file-req_no = lv_req_no.
    lw_file-filename = lv_file.
    lw_file-value = is_media_resource-value.
    lw_file-mimetype = is_media_resource-mime_type.
    lw_file-sydate = sy-datum.
    lw_file-sytime = sy-uzeit.
    INSERT INTO zebr_t_file VALUES lw_file.
    COMMIT WORK.
    CLEAR lw_file.
  ENDMETHOD.


  METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~DELETE_STREAM.
    DATA: lw_file TYPE zebr_t_file.
    FIELD-SYMBOLS:<fs_key> TYPE /iwbep/s_mgw_name_value_pair.
    READ TABLE it_key_tab ASSIGNING <fs_key> INDEX 1.
    IF sy-subrc EQ 0.
      DATA(lv_req_no) = <fs_key>-value.
    ENDIF.
    READ TABLE it_key_tab ASSIGNING <fs_key> INDEX 2.
    IF sy-subrc EQ 0.
      DATA(lv_file) = <fs_key>-value.
      CALL METHOD cl_http_utility=>unescape_url
          EXPORTING
            escaped   = lv_file
          RECEIVING
            unescaped = lv_file.
    ENDIF.
    lw_file-req_no = lv_req_no.
    lw_file-filename = lv_file.

    DELETE zebr_t_file FROM lw_file.
    CLEAR lw_file.
  ENDMETHOD.


  METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY
**  EXPORTING
**    iv_entity_name          =
**    iv_entity_set_name      =
**    iv_source_name          =
**    it_key_tab              =
**    it_navigation_path      =
**    io_tech_request_context =
**  IMPORTING
**    er_entity               =
**    es_response_context     =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    DATA : ls_partinfo        TYPE zcl_zebr_form_mpc_ext=>ts_ebrpartinfo,
           ls_header          TYPE zcl_zebr_form_mpc_ext=>ts_ebrheader,
           ls_ebrchat         TYPE zcl_zebr_form_mpc_ext=>ts_ebrchat,
           ls_mfrpn           TYPE  zcl_zebr_form_mpc_ext=>ts_ebrmfrpn,
           ls_bndno           TYPE  zcl_zebr_form_mpc_ext=>ts_ebrbndno,
           ls_ebrdieqty       TYPE  zcl_zebr_form_mpc_ext=>ts_ebrdieqty,
           ls_ebrreqtypevalue TYPE  zcl_zebr_form_mpc_ext=>ts_ebrreqtypevalue,
           lcl_ebr            TYPE  REF TO zcl_zebr_form_dpc_ext,
           lo_exception       TYPE REF TO /iwbep/cx_mgw_tech_exception,
           ls_ebrfile         TYPE  zcl_zebr_form_mpc_ext=>ts_ebrfile.

    CREATE OBJECT lcl_ebr.
    " send data  to deep entity
    IF iv_entity_name EQ 'EBRPartInfo' .
      TRY.
          CALL METHOD lcl_ebr->ebrpartinfoset_get_entity
            EXPORTING
              iv_entity_name          = iv_entity_name
              iv_entity_set_name      = iv_entity_set_name
              iv_source_name          = iv_source_name
              it_key_tab              = it_key_tab
              it_navigation_path      = it_navigation_path
              io_tech_request_context = io_tech_request_context
            IMPORTING
              er_entity               = ls_partinfo
              es_response_context     = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception   INTO  lo_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = ls_partinfo
           CHANGING
           cr_data = er_entity ).
    ELSEIF iv_entity_name EQ 'EBRHeader'  .
*      TRY.
*          CALL METHOD lcl_ebr->ebrheader_get_entity
*            EXPORTING
*              iv_entity_name          = iv_entity_name
*              iv_entity_set_name      = iv_entity_set_name
*              iv_source_name          = iv_source_name
*              it_key_tab              = it_key_tab
*              it_navigation_path      = it_navigation_path
*              io_tech_request_context = io_tech_request_context
*            IMPORTING
*              er_entity               = ls_header
*              es_response_context     = es_response_context.
*        CATCH /iwbep/cx_mgw_busi_exception .
*        CATCH /iwbep/cx_mgw_tech_exception .
*      ENDTRY.
*
*      copy_data_to_ref(
*           EXPORTING
*           is_data = ls_header
*           CHANGING
*           cr_data = er_entity ).
    ELSEIF iv_entity_name EQ 'EBRChat' .
      TRY.
          CALL METHOD lcl_ebr->ebrchatset_get_entity
            EXPORTING
              iv_entity_name          = iv_entity_name
              iv_entity_set_name      = iv_entity_set_name
              iv_source_name          = iv_source_name
              it_key_tab              = it_key_tab
              it_navigation_path      = it_navigation_path
              io_tech_request_context = io_tech_request_context
            IMPORTING
              er_entity               = ls_ebrchat
              es_response_context     = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception   .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = ls_ebrchat
           CHANGING
           cr_data = er_entity ).

    ELSEIF iv_entity_name EQ 'EBRMfrpn' .
      TRY.
          CALL METHOD lcl_ebr->ebrmfrpnset_get_entity
            EXPORTING
              iv_entity_name          = iv_entity_name
              iv_entity_set_name      = iv_entity_set_name
              iv_source_name          = iv_source_name
              it_key_tab              = it_key_tab
              it_navigation_path      = it_navigation_path
              io_tech_request_context = io_tech_request_context
            IMPORTING
              er_entity               = ls_mfrpn
              es_response_context     = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception  INTO  lo_exception.
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = ls_mfrpn
           CHANGING
           cr_data = er_entity ).

    ELSEIF iv_entity_name EQ 'EBRBndNo' .
      TRY.
          CALL METHOD lcl_ebr->ebrbndnoset_get_entity
            EXPORTING
              iv_entity_name          = iv_entity_name
              iv_entity_set_name      = iv_entity_set_name
              iv_source_name          = iv_source_name
              it_key_tab              = it_key_tab
              it_navigation_path      = it_navigation_path
              io_tech_request_context = io_tech_request_context
            IMPORTING
              er_entity               = ls_bndno
              es_response_context     = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = ls_bndno
           CHANGING
           cr_data = er_entity ).

    ELSEIF iv_entity_name EQ 'EBRFile' .
      TRY.
          CALL METHOD lcl_ebr->ebrfileset_get_entity
            EXPORTING
              iv_entity_name          = iv_entity_name
              iv_entity_set_name      = iv_entity_set_name
              iv_source_name          = iv_source_name
              it_key_tab              = it_key_tab
              it_navigation_path      = it_navigation_path
              io_tech_request_context = io_tech_request_context
            IMPORTING
              er_entity               = ls_ebrfile
              es_response_context     = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = ls_ebrfile
           CHANGING
           cr_data = er_entity ).
    ELSEIF iv_entity_name EQ 'EBRDieQty' .
      TRY.
          CALL METHOD lcl_ebr->ebrdieqtyset_get_entity
            EXPORTING
              iv_entity_name          = iv_entity_name
              iv_entity_set_name      = iv_entity_set_name
              iv_source_name          = iv_source_name
              it_key_tab              = it_key_tab
              it_navigation_path      = it_navigation_path
              io_tech_request_context = io_tech_request_context
            IMPORTING
              er_entity               = ls_ebrdieqty
              es_response_context     = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data =   ls_ebrdieqty
           CHANGING
           cr_data = er_entity ).
    ELSEIF iv_entity_name EQ 'EBRReqTypeValue' .
      TRY.
          CALL METHOD lcl_ebr->ebrreqtypevalues_get_entity
            EXPORTING
              iv_entity_name          = iv_entity_name
              iv_entity_set_name      = iv_entity_set_name
              iv_source_name          = iv_source_name
              it_key_tab              = it_key_tab
              it_navigation_path      = it_navigation_path
              io_tech_request_context = io_tech_request_context
            IMPORTING
              er_entity               = ls_ebrreqtypevalue
              es_response_context     = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data =   ls_ebrreqtypevalue
           CHANGING
           cr_data = er_entity ).

    ENDIF.
    IF lo_exception IS BOUND .
      RAISE EXCEPTION lo_exception.
      RETURN.
    ENDIF .
  ENDMETHOD.


  METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET.
    DATA : lt_matsh        TYPE zcl_zebr_form_mpc_ext=>tt_ebrmatsh,
           lt_lotsh        TYPE zcl_zebr_form_mpc_ext=>tt_ebrlotsh,
           lt_lotinfo      TYPE zcl_zebr_form_mpc_ext=>tt_ebrlotinfo,
           lt_assyfac      TYPE zcl_zebr_form_mpc_ext=>tt_ebrassyfac,
           lt_bumpfac      TYPE zcl_zebr_form_mpc_ext=>tt_ebrbumpfac,
           lt_finishfac    TYPE zcl_zebr_form_mpc_ext=>tt_ebrfinishfac,
           lt_testfac      TYPE zcl_zebr_form_mpc_ext=>tt_ebrtestfac,
           lt_pbsfac       TYPE zcl_zebr_form_mpc_ext=>tt_ebrpbsfac,
           lt_reqtype      TYPE zcl_zebr_form_mpc_ext=>tt_ebrreqtype,
           lt_ebrheader    TYPE zcl_zebr_form_mpc_ext=>tt_ebrheader,
           lt_ebrgetfac    TYPE zcl_zebr_form_mpc_ext=>tt_ebrgetfacility,
           lt_ebrnotif     TYPE zcl_zebr_form_mpc_ext=>tt_ebrnotification,
           lt_ebrdshipfac  TYPE zcl_zebr_form_mpc_ext=>tt_ebrdshipfac,
           lt_ebrwafersort TYPE zcl_zebr_form_mpc_ext=>tt_ebrwafersort,
           lt_ebrshipto    TYPE zcl_zebr_form_mpc_ext=>tt_ebrshipto,
           lt_ebrfasttrack TYPE zcl_zebr_form_mpc_ext=>tt_ebrfasttrack,
           lt_ebrstatuslog TYPE zcl_zebr_form_mpc_ext=>tt_ebrstatuslog,
           lt_ebrmaster    TYPE zcl_zebr_form_mpc_ext=>tt_ebrmasterstatus,
           lt_addpart      TYPE zcl_zebr_form_mpc_ext=>tt_ebraddpart,
           lt_waferno      TYPE zcl_zebr_form_mpc_ext=>tt_ebrwaferno,
           lt_billcode     TYPE zcl_zebr_form_mpc_ext=>tt_ebrbillcode,
           lt_dcfac        TYPE zcl_zebr_form_mpc_ext=>tt_ebrdcfac,
           lt_cartype      TYPE zcl_zebr_form_mpc_ext=>tt_ebrcarriertype,
           lt_lifnr        TYPE zcl_zebr_form_mpc_ext=>tt_ebrvendorsh,
           lt_prf          TYPE zcl_zebr_form_mpc_ext=>tt_ebrprfsh,
           lt_rel          TYPE zcl_zebr_form_mpc_ext=>tt_ebrrelsh,
           lt_file         TYPE zcl_zebr_form_mpc_ext=>tt_ebrfile,
           lt_ebrnppdet    type zcl_zebr_form_mpc_ext=>tt_ebrnppdet,
           lt_ebrdreason   type zcl_zebr_form_mpc_ext=>tt_ebrdiscardreason,
           lt_ebrpcntype   type zcl_zebr_form_mpc_ext=>tt_ebrpcntype,
           lcl_ebr         TYPE  REF TO zcl_zebr_form_dpc_ext,


           lo_exception    TYPE REF TO /iwbep/cx_mgw_tech_exception.
    CREATE OBJECT lcl_ebr.

    IF iv_entity_name EQ 'EBRMatSh' .
      TRY .
          CALL METHOD lcl_ebr->ebrmatsearchset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_matsh
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_matsh
           CHANGING
           cr_data = er_entityset ).

    ELSEIF iv_entity_name EQ 'EBRLotSh' .


      TRY .
          CALL METHOD lcl_ebr->ebrlotsearchset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_lotsh
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_lotsh
           CHANGING
           cr_data = er_entityset ).

    ELSEIF iv_entity_name EQ 'EBRLotInfo' .
      TRY .
          CALL METHOD lcl_ebr->ebrlotinfoset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_lotinfo
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_lotinfo
           CHANGING
           cr_data = er_entityset ).

    ELSEIF iv_entity_name EQ 'EBRAssyFac' .
      TRY .
          CALL METHOD lcl_ebr->ebrassyfacset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_assyfac
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_assyfac
           CHANGING
           cr_data = er_entityset ).

    ELSEIF iv_entity_name EQ 'EBRBumpFac' .
      TRY .
          CALL METHOD lcl_ebr->ebrbumpfacset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_bumpfac
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_bumpfac
           CHANGING
           cr_data = er_entityset ).

    ELSEIF iv_entity_name EQ 'EBRPbsFac' .


      TRY .
          CALL METHOD lcl_ebr->ebrpbsfacset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_pbsfac
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_pbsfac
           CHANGING
           cr_data = er_entityset ).

    ELSEIF iv_entity_name EQ 'EBRTestFac' .


      TRY .
          CALL METHOD lcl_ebr->ebrtestfacset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_testfac
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_testfac
           CHANGING
           cr_data = er_entityset ).

    ELSEIF iv_entity_name EQ 'EBRFinishfac' .


      TRY .
          CALL METHOD lcl_ebr->ebrfinishfacset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_finishfac
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_finishfac
           CHANGING
           cr_data = er_entityset ).

    ELSEIF iv_entity_name EQ 'EBRReqType' .


      TRY .
          CALL METHOD lcl_ebr->ebrreqtypeset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_reqtype
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception  INTO  lo_exception.
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_reqtype
           CHANGING
           cr_data = er_entityset ).
    ELSEIF iv_entity_name EQ 'EBRHeader' .
      TRY.
          CALL METHOD lcl_ebr->ebrheaderset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_ebrheader
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.
      copy_data_to_ref(
       EXPORTING
       is_data =  lt_ebrheader
       CHANGING
       cr_data = er_entityset ).
    ELSEIF  iv_entity_name  = 'EBRGetFacility'  .

      TRY .
          CALL METHOD lcl_ebr->ebrgetfacilityse_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_ebrgetfac
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_ebrgetfac
           CHANGING
           cr_data = er_entityset ).

    ELSEIF iv_entity_name  = 'EBRNotification' .
      TRY .
          CALL METHOD lcl_ebr->ebrnotifications_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_ebrnotif
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_ebrnotif
           CHANGING
           cr_data = er_entityset ).

    ELSEIF iv_entity_name  = 'EBRDShipFac'.
      TRY .
          CALL METHOD lcl_ebr->ebrdshipfacset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_ebrdshipfac
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_ebrdshipfac
           CHANGING
           cr_data = er_entityset ).
    ELSEIF iv_entity_name  = 'EBRWaferSort'.
      TRY .
          CALL METHOD lcl_ebr->ebrwafersortset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_ebrwafersort
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_ebrwafersort
           CHANGING
           cr_data = er_entityset ).
    ELSEIF iv_entity_name  = 'EBRShipto'.
      TRY .
          CALL METHOD lcl_ebr->ebrshiptoset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_ebrshipto
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_ebrshipto
           CHANGING
           cr_data = er_entityset ).
    ELSEIF iv_entity_name  = 'EBRFastTrack'.
      TRY .
          CALL METHOD lcl_ebr->ebrfasttrackset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_ebrfasttrack
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_ebrfasttrack
           CHANGING
           cr_data = er_entityset ).

    ELSEIF  iv_entity_name  = 'EBRStatusLog'.
      TRY .
          CALL METHOD lcl_ebr->ebrstatuslogset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_ebrstatuslog
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_ebrstatuslog
           CHANGING
           cr_data = er_entityset ).
    ELSEIF iv_entity_name EQ 'EBRMasterStatus' .


      TRY .
          CALL METHOD lcl_ebr->ebrmasterstatuss_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_ebrmaster
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.
      copy_data_to_ref(
     EXPORTING
     is_data = lt_ebrmaster
     CHANGING
     cr_data = er_entityset ).
    ELSEIF iv_entity_name EQ 'EBRAddPart' .


      TRY .
          CALL METHOD lcl_ebr->ebraddpartset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_addpart
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_addpart
           CHANGING
           cr_data = er_entityset ).
    ELSEIF iv_entity_name EQ 'EBRWaferNo' .


      TRY .
          CALL METHOD lcl_ebr->ebrwafernoset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_waferno
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_waferno
           CHANGING
           cr_data = er_entityset ).
    ELSEIF iv_entity_name EQ 'EBRBillCode' .


      TRY .
          CALL METHOD lcl_ebr->ebrbillcodeset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_billcode
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_billcode
           CHANGING
           cr_data = er_entityset ).


    ELSEIF iv_entity_name EQ 'EBRDCFac' .
      TRY .
          CALL METHOD lcl_ebr->ebrdcfacset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_dcfac
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_dcfac
           CHANGING
           cr_data = er_entityset ).
    ELSEIF iv_entity_name EQ 'EBRCarrierType' .
      TRY .
          CALL METHOD lcl_ebr->ebrcarriertypese_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_cartype
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_cartype
           CHANGING
           cr_data = er_entityset ).

    ELSEIF iv_entity_name EQ 'EBRVendorSh' .


      TRY .
          CALL METHOD lcl_ebr->ebrvendorset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_lifnr
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_lifnr
           CHANGING
           cr_data = er_entityset ).
    ELSEIF iv_entity_name EQ 'EBRPrfSh' .


      TRY .
          CALL METHOD lcl_ebr->ebrprfset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_prf
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_prf
           CHANGING
           cr_data = er_entityset ).


    ELSEIF iv_entity_name EQ 'EBRRelSh' .


      TRY .
          CALL METHOD lcl_ebr->ebrrelset_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = lt_rel
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception .
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      copy_data_to_ref(
           EXPORTING
           is_data = lt_rel
           CHANGING
           cr_data = er_entityset ).

      ELSEIF iv_entity_name EQ 'EBRFile' .
        TRY .
            CALL METHOD lcl_ebr->ebrfileset_get_entityset
              EXPORTING
                iv_entity_name           = iv_entity_name
                iv_entity_set_name       = iv_entity_set_name
                iv_source_name           = iv_source_name
                it_filter_select_options = it_filter_select_options
                is_paging                = is_paging
                it_key_tab               = it_key_tab
                it_navigation_path       = it_navigation_path
                it_order                 = it_order
                iv_filter_string         = iv_filter_string
                iv_search_string         = iv_search_string
                io_tech_request_context  = io_tech_request_context
              IMPORTING
                et_entityset             = lt_file
                es_response_context      = es_response_context.
          CATCH /iwbep/cx_mgw_busi_exception .
          CATCH /iwbep/cx_mgw_tech_exception .
        ENDTRY.

        copy_data_to_ref(
             EXPORTING
             is_data = lt_file
             CHANGING
             cr_data = er_entityset ).

     ELSEIF iv_entity_name EQ 'EBRNppDet' .
        TRY .
            CALL METHOD lcl_ebr->EBRNPPDETSET_GET_ENTITYSET
              EXPORTING
                iv_entity_name           = iv_entity_name
                iv_entity_set_name       = iv_entity_set_name
                iv_source_name           = iv_source_name
                it_filter_select_options = it_filter_select_options
                is_paging                = is_paging
                it_key_tab               = it_key_tab
                it_navigation_path       = it_navigation_path
                it_order                 = it_order
                iv_filter_string         = iv_filter_string
                iv_search_string         = iv_search_string
                io_tech_request_context  = io_tech_request_context
              IMPORTING
                et_entityset             = lt_ebrnppdet
                es_response_context      = es_response_context.
          CATCH /iwbep/cx_mgw_busi_exception .
          CATCH /iwbep/cx_mgw_tech_exception .
        ENDTRY.

        copy_data_to_ref(
             EXPORTING
             is_data = lt_ebrnppdet
             CHANGING
             cr_data = er_entityset ).

   ELSEIF iv_entity_name EQ 'EBRDiscardReason' .
        TRY .
            CALL METHOD lcl_ebr->EBRDISCARDREASON_GET_ENTITYSET
              EXPORTING
                iv_entity_name           = iv_entity_name
                iv_entity_set_name       = iv_entity_set_name
                iv_source_name           = iv_source_name
                it_filter_select_options = it_filter_select_options
                is_paging                = is_paging
                it_key_tab               = it_key_tab
                it_navigation_path       = it_navigation_path
                it_order                 = it_order
                iv_filter_string         = iv_filter_string
                iv_search_string         = iv_search_string
                io_tech_request_context  = io_tech_request_context
              IMPORTING
                et_entityset             = lt_ebrdreason
                es_response_context      = es_response_context.
          CATCH /iwbep/cx_mgw_busi_exception .
          CATCH /iwbep/cx_mgw_tech_exception .
        ENDTRY.

        copy_data_to_ref(
             EXPORTING
             is_data = lt_ebrdreason
             CHANGING
             cr_data = er_entityset ).

    ELSEIF iv_entity_name EQ 'EBRPcnType' .
        TRY .
            CALL METHOD lcl_ebr->EBRPCNTYPESET_GET_ENTITYSET
              EXPORTING
                iv_entity_name           = iv_entity_name
                iv_entity_set_name       = iv_entity_set_name
                iv_source_name           = iv_source_name
                it_filter_select_options = it_filter_select_options
                is_paging                = is_paging
                it_key_tab               = it_key_tab
                it_navigation_path       = it_navigation_path
                it_order                 = it_order
                iv_filter_string         = iv_filter_string
                iv_search_string         = iv_search_string
                io_tech_request_context  = io_tech_request_context
              IMPORTING
                et_entityset             = lt_ebrpcntype
                es_response_context      = es_response_context.
          CATCH /iwbep/cx_mgw_busi_exception .
          CATCH /iwbep/cx_mgw_tech_exception .
        ENDTRY.

        copy_data_to_ref(
             EXPORTING
             is_data = lt_ebrpcntype
             CHANGING
             cr_data = er_entityset ).



    ENDIF.

    IF lo_exception IS  BOUND .
      RAISE EXCEPTION lo_exception.
      RETURN.
    ENDIF .
  ENDMETHOD.


  METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY.
    DATA : lv_reqno  TYPE zebr_t_header-req_no .
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY
**  EXPORTING
**    iv_entity_name           =
**    iv_entity_set_name       =
**    iv_source_name           =
**    it_key_tab               =
**    it_navigation_path       =
**    io_expand                =
**    io_tech_request_context  =
**  IMPORTING
**    er_entity                =
**    es_response_context      =
**    et_expanded_clauses      =
**    et_expanded_tech_clauses =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    DATA : ls_ebrheader  TYPE zcl_zebr_form_mpc_ext=>ts_part_lot  .
    READ TABLE it_key_tab INTO DATA(ls_key_tab)  WITH KEY name  = 'ReqNo'  .
    IF sy-subrc EQ 0.
      lv_reqno  = ls_key_tab-value  .
      SELECT  SINGLE  * FROM  zebr_t_header  INTO @DATA(ls_header)  WHERE req_no  = @lv_reqno .
      IF ls_header IS NOT INITIAL .
        ls_ebrheader-req_no                                          =     ls_header-req_no                  .
        ls_ebrheader-req_type                                        =     ls_header-req_type                  .
        ls_ebrheader-req_status                                      =     ls_header-req_status                  .
        ls_ebrheader-mpw_wafer                                       =     ls_header-mpw_wafer                  .
        ls_ebrheader-hybrid                                          =     ls_header-hybrid   .
        ls_ebrheader-output_part                                     =     ls_header-output_part                  .
        ls_ebrheader-mfrpn                                           =     ls_header-mfrpn                  .
        ls_ebrheader-status                                          =     ls_header-status                  .
        ls_ebrheader-group_ebr                                       =     ls_header-group_ebr                  .
        ls_ebrheader-bu_name                                         =     ls_header-bu_name                  .
        ls_ebrheader-test_group                                      =     ls_header-test_group                  .
        ls_ebrheader-package_type                                    =     ls_header-package_type                  .
        ls_ebrheader-package_code                                    =     ls_header-package_code                  .
        ls_ebrheader-die_type                                        =     ls_header-die_type                  .
        ls_ebrheader-dierevcode                                      =     ls_header-dierevcode                  .
        ls_ebrheader-sample_marking                                  =     ls_header-sample_marking                  .
        ls_ebrheader-additional_marking                              =     ls_header-additional_marking                  .
        ls_ebrheader-wafer_sort                                      =     ls_header-wafer_sort                  .
        ls_ebrheader-ship_to                                         =     ls_header-ship_to                  .
        ls_ebrheader-nprf_no                                         =     ls_header-nprf_no                  .
        ls_ebrheader-nfo                                             =     ls_header-nfo                  .
        ls_ebrheader-swr                                             =     ls_header-swr                  .
        ls_ebrheader-fasttrack                                       =     ls_header-fasttrack                  .
        ls_ebrheader-fasttrack_no                                    =     ls_header-fasttrack_no                  .
        ls_ebrheader-drop_ship                                       =     ls_header-drop_ship                  .
        ls_ebrheader-dc_location                                     =     ls_header-dc_location                  .
        ls_ebrheader-assy_instr                                      =     ls_header-assy_instr                  .
        ls_ebrheader-npp_processor                                   =     ls_header-npp_processor                  .
        ls_ebrheader-issue_assy_team                                 =     ls_header-issue_assy_team                  .
*        ls_ebrheader-ISSUE_ASSY_SET_DATE                             =     ls_header-ISSUE_ASSY_SET_DATE                  .
*        ls_ebrheader-ISSUE_ASSY_COM_DATE                             =     ls_header-ISSUE_ASSY_COM_DATE                  .
        ls_ebrheader-issue_pdm_team                                  =     ls_header-issue_pdm_team                  .
*        ls_ebrheader-ISSUE_PDM_SET_DATE                              =     ls_header-ISSUE_PDM_SET_DATE                  .
*        ls_ebrheader-ISSUE_PDM_COM_DATE                              =     ls_header-ISSUE_PDM_COM_DATE                  .
        ls_ebrheader-masterdata_status                               =     ls_header-masterdata_status                  .
        ls_ebrheader-supply_status                                   =     ls_header-supply_status                  .
        ls_ebrheader-created_by                                      =     ls_header-created_by                  .
        ls_ebrheader-created_on                                      =     ls_header-created_on                  .
        ls_ebrheader-changed_by                                      =     ls_header-changed_by .
        ls_ebrheader-changed_on                                      =     ls_header-changed_on .


        SELECT  * FROM  zebr_t_lotinfo INTO TABLE @DATA(lt_lotinfo)  WHERE req_no   = @lv_reqno .
        IF sy-subrc EQ 0.
          LOOP AT lt_lotinfo   INTO DATA(ls_lotinfo)  .
            APPEND INITIAL LINE TO ls_ebrheader-ebrheadlotnav   ASSIGNING FIELD-SYMBOL(<fs_ebrlotnav>)  .
            MOVE-CORRESPONDING  ls_lotinfo   TO <fs_ebrlotnav> .
          ENDLOOP .
        ENDIF.



        SELECT * FROM zebr_t_notif  INTO TABLE @DATA(lt_notif)  WHERE req_no  = @lv_reqno  .                   "#EC CI_ALL_FIELDS_NEEDED
        LOOP AT ls_ebrheader-ebrnotifnav  INTO DATA(ls_ebrnotifnav)  .
          APPEND INITIAL LINE TO  ls_ebrheader-ebrnotifnav ASSIGNING FIELD-SYMBOL(<fs_ebrnotifnav>)  .
          MOVE-CORRESPONDING  ls_ebrnotifnav  TO <fs_ebrnotifnav>  .
        ENDLOOP.

      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
**  EXPORTING
**    iv_entity_name           =
**    iv_entity_set_name       =
**    iv_source_name           =
**    it_filter_select_options =
**    it_order                 =
**    is_paging                =
**    it_navigation_path       =
**    it_key_tab               =
**    iv_filter_string         =
**    iv_search_string         =
**    io_expand                =
**    io_tech_request_context  =
**  IMPORTING
**    er_entityset             =
**    et_expanded_clauses      =
**    et_expanded_tech_clauses =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    IF iv_entity_name  = 'EBRHeader'  .
      DATA : lv_reqno          TYPE zebr_t_header-req_no,
             lrt_reqno         TYPE RANGE OF zebr_t_header-req_no,
             ls_ebrheader      TYPE zcl_zebr_form_mpc_ext=>ts_part_lot,
             lv_name           TYPE thead-tdname,
             lt_bom_details    TYPE ztt_bom_item,
             lt_stpo           TYPE ztt_bom_com_level,
             lt_snum_asy       TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
             lt_scurr_asy      TYPE STANDARD TABLE OF bapi1003_alloc_values_curr,
             lt_schar_asy      TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
             lt_return_asy     TYPE bapiret2_t,
             lt_bom_output     TYPE ztt_bom_ip_out,
             lv_copied_flag(1),
             lt_ebrheader      TYPE STANDARD TABLE OF zcl_zebr_form_mpc_ext=>ts_part_lot.

      CLEAR lv_copied_flag.

      LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options)  .
        IF ls_filter_select_options-property  = 'ReqNo' .
          CLEAR lv_copied_flag.
          READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_reqno) INDEX 1 .
          IF sy-subrc EQ  0 .
            TRANSLATE  ls_filter_reqno-low  TO UPPER CASE .
            TRANSLATE  ls_filter_reqno-high  TO UPPER CASE .
            APPEND INITIAL LINE TO lrt_reqno ASSIGNING FIELD-SYMBOL(<fr_reqno>).
            <fr_reqno>-sign  =  ls_filter_reqno-sign .
            <fr_reqno>-option = ls_filter_reqno-option  .
            <fr_reqno>-low = ls_filter_reqno-low  .
            <fr_reqno>-high = ls_filter_reqno-high  .
          ENDIF.
        ENDIF.

        IF ls_filter_select_options-property  = 'CopiedEbr' .
          lv_copied_flag = 'X'.
          READ TABLE ls_filter_select_options-select_options  INTO ls_filter_reqno INDEX 1 .
          IF sy-subrc EQ  0 .
            TRANSLATE  ls_filter_reqno-low  TO UPPER CASE .
            TRANSLATE  ls_filter_reqno-high  TO UPPER CASE .
            APPEND INITIAL LINE TO lrt_reqno ASSIGNING <fr_reqno>.
            <fr_reqno>-sign  =  ls_filter_reqno-sign .
            <fr_reqno>-option = ls_filter_reqno-option  .
            <fr_reqno>-low = ls_filter_reqno-low  .
            <fr_reqno>-high = ls_filter_reqno-high  .
          ENDIF.
        ENDIF.
      ENDLOOP.


      SELECT  SINGLE  * FROM  zebr_t_header  INTO @DATA(ls_header)  WHERE req_no IN @lrt_reqno.
      IF ls_header IS NOT INITIAL .
        ls_ebrheader-req_no                                          =     ls_header-req_no                  .
        ls_ebrheader-req_type                                        =     ls_header-req_type                .
        ls_ebrheader-req_type_desc                                   =     ls_header-req_type_desc             .
        IF lv_copied_flag = 'X'.
          " get requestor  and group
          SELECT SINGLE bname , accnt   FROM usr02  INTO @DATA(ls_usrc)  WHERE bname = @sy-uname .
          IF ls_usrc IS NOT INITIAL .
            SELECT  SINGLE employee_id ,displayname , maximorg  FROM zworkday_data  INTO @DATA(ls_workdayc) WHERE  employee_id = @ls_usrc-accnt .
            ls_header-created_by = ls_workdayc-displayname.
            ls_header-changed_by = ls_workdayc-displayname.
          ENDIF .
          ls_header-created_on   = ls_header-changed_on = sy-datum.
          ls_header-created_time = ls_header-changed_time = sy-uzeit.
          ls_header-req_status_no  =     '00'.
          CLEAR : ls_header-npp_processor ,
                  ls_header-masterdata_status,
                  ls_header-supply_status ,
                  ls_header-lot_induct.

        ENDIF.
        ls_ebrheader-req_status_no                                   =     ls_header-req_status_no.
        ls_ebrheader-created_by                                      =     ls_header-created_by.
        ls_ebrheader-changed_by                                      =    ls_header-changed_by.


        SELECT SINGLE  field2  FROM zebr_config  INTO  ls_ebrheader-req_status    WHERE name  = 'REQ_STATUS'
                                                                                 AND type  =  'DROP_DOWN'
                                                                                AND field1  =  ls_ebrheader-req_status_no .
        ls_ebrheader-mpw_wafer                                       =     ls_header-mpw_wafer                  .
        ls_ebrheader-hybrid                                          =     ls_header-hybrid                   .
        ls_ebrheader-output_part                                     =     ls_header-output_part                  .
        ls_ebrheader-mfrpn                                           =     ls_header-mfrpn                  .
        ls_ebrheader-status                                          =     ls_header-status                  .
        ls_ebrheader-group_ebr                                       =     ls_header-group_ebr                  .
        ls_ebrheader-bu_name                                         =     ls_header-bu_name                  .
        ls_ebrheader-test_group                                      =     ls_header-test_group                  .
        ls_ebrheader-package_type                                    =     ls_header-package_type                  .
        ls_ebrheader-package_code                                    =     ls_header-package_code                  .
        ls_ebrheader-die_type                                        =     ls_header-die_type                  .
        ls_ebrheader-dierevcode                                      =     ls_header-dierevcode                  .
        ls_ebrheader-sample_marking                                  =     ls_header-sample_marking                  .
        ls_ebrheader-additional_marking                              =     ls_header-additional_marking                  .
        ls_ebrheader-wafer_sort                                      =     ls_header-wafer_sort                  .
        ls_ebrheader-ship_to                                         =     ls_header-ship_to                  .
        ls_ebrheader-nprf_no                                         =     ls_header-nprf_no                  .
        ls_ebrheader-nfo                                             =     ls_header-nfo                  .
        ls_ebrheader-swr                                             =     ls_header-swr                  .
        ls_ebrheader-fasttrack                                       =     ls_header-fasttrack                  .
        ls_ebrheader-fasttrack_no                                    =     ls_header-fasttrack_no                  .
        ls_ebrheader-drop_ship                                       =     ls_header-drop_ship                  .
        ls_ebrheader-dc_location                                     =     ls_header-dc_location                  .
        ls_ebrheader-assy_instr                                      =     ls_header-assy_instr                  .
        ls_ebrheader-issue_assy_team                                 =     ls_header-issue_assy_team                  .
        ls_ebrheader-assy_carrier_type                               =      ls_header-assy_carrier_type   .
        ls_ebrheader-test_carrier_type                               =      ls_header-test_carrier_type   .
*        ls_ebrheader-ISSUE_ASSY_SET_DATE                             =     ls_header-ISSUE_ASSY_SET_DATE                  .
*        ls_ebrheader-ISSUE_ASSY_COM_DATE                             =     ls_header-ISSUE_ASSY_COM_DATE                  .
        ls_ebrheader-issue_pdm_team                                  =     ls_header-issue_pdm_team                  .
*        ls_ebrheader-ISSUE_PDM_SET_DATE                              =     ls_header-ISSUE_PDM_SET_DATE                  .
*        ls_ebrheader-ISSUE_PDM_COM_DATE                              =     ls_header-ISSUE_PDM_COM_DATE                  .
        ls_ebrheader-masterdata_status                               =     ls_header-masterdata_status                  .
        ls_ebrheader-supply_status                                   =     ls_header-supply_status                  .
        ls_ebrheader-add_mark_char                                   =    ls_header-add_mark_char     .
        ls_ebrheader-qa_buy_off                                      =    ls_header-qa_buy_off  .
        ls_ebrheader-created_on                                      =    ls_header-created_on                  .
        ls_ebrheader-changed_on                                      =    ls_header-changed_on      .
        ls_ebrheader-changed_time                                    =    ls_header-changed_time  .
        ls_ebrheader-created_time                                    =    ls_header-created_time .
        ls_ebrheader-req_final                                       =    ls_header-req_final                    .
        ls_ebrheader-req_pcn                                         =    ls_header-req_pcn                    .
        ls_ebrheader-req_rel                                         =    ls_header-req_rel                    .
        ls_ebrheader-apr_no                                          =    ls_header-apr_no                    .
        ls_ebrheader-pcn_no                                          =    ls_header-pcn_no                    .
        ls_ebrheader-relid                                           =    ls_header-relid                    .
        ls_ebrheader-bill_code                                       =    ls_header-bill_code                .
        ls_ebrheader-lot_induct                                      =    ls_header-lot_induct  .
        ls_ebrheader-ship_to_chng                                    =    ls_header-ship_to_chng             .
        ls_ebrheader-wf_chng_flag                                    =    ls_header-wf_chng_flag             .
        ls_ebrheader-carr_chng_flag                                  =    ls_header-carr_chng_flag           .
        ls_ebrheader-npp_processor                                   =    ls_header-npp_processor            .
        ls_ebrheader-new_bom                                         =    ls_header-new_bom  .
        ls_ebrheader-new_bnd                                         =    ls_header-new_bnd  .
        ls_ebrheader-doe                                             =    ls_header-doe      .
        ls_ebrheader-add_spec_top                                    =    ls_header-add_spec_top  .
        ls_ebrheader-att_map                                         =    ls_header-att_map       .
        ls_ebrheader-skip_skelton                                    =    ls_header-skip_skelton  .
        ls_ebrheader-split_lot                                       =    ls_header-split_lot     .
        ls_ebrheader-npi_check                                       =    ls_header-npi_check     .
        ls_ebrheader-swr_sar                                         =    ls_header-swr_sar       .
        ls_ebrheader-ship_to_chng                                    =    'MPOC'  .
        ls_ebrheader-wf_chng_flag                                    =    'SORT'  .
        ls_ebrheader-thin_wlp_flag                                   =    ls_header-thin_wlp_flag      .
        ls_ebrheader-krpw_flag                                       =    ls_header-krpw_flag     .
        ls_ebrheader-pre_qual                                        =    ls_header-pre_qual         .
        ls_ebrheader-pre_qual_ft                                     =    ls_header-pre_qual_ft      .
        ls_ebrheader-assy_buy_off                                    =    ls_header-assy_buy_off     .
        ls_ebrheader-assy_ft_reqd                                    =    ls_header-assy_ft_reqd     .
        ls_ebrheader-pcn_type                                        =    ls_header-pcn_type         .

* Begin of change JGAND37207
        ls_ebrheader-lot_in_db  = ls_header-lot_in_db.
        ls_ebrheader-ws_out_date  =  ls_header-ws_out_date.
* End of change JGAND37207
*        ls_ebrheader-discard_reason                                  =    ls_header-discard_reason  .
        " get default value for assembly carrier type
        " get bom numberf rom mast table basen on  material number form screen
        SELECT SINGLE matnr ,mtart , bismt ,mfrpn FROM mara INTO  @DATA(ls_mara)  WHERE mfrpn = @ls_ebrheader-mfrpn .
        IF sy-subrc EQ 0.
          IF ls_mara-mtart  = 'FERT'.
            DATA(lv_fert_matnr)   = ls_mara-matnr .
          ELSE.

            CALL FUNCTION 'ZGET_MULTILEVEL_BOM_BOTTOM_UP'
              EXPORTING
                im_matnr         = ls_mara-matnr
              CHANGING
                ct_bom_details   = lt_bom_details
                ct_bom_input_out = lt_bom_output.


            DELETE lt_bom_details  WHERE zout_mtart NE 'FERT'  .
            DELETE lt_bom_output    WHERE zout_mtart NE 'FERT'  .
            SELECT matnr , matkl FROM mara INTO TABLE @DATA(lt_mara_matgrp)
                      FOR ALL ENTRIES IN @lt_bom_output  WHERE matnr  = @lt_bom_output-zout_mat .
            IF sy-subrc EQ 0.
              DELETE  lt_mara_matgrp WHERE ( matkl  = 'F005'  OR matkl = 'F009'  OR matkl  = 'F010'  )  .
            ENDIF.
            READ TABLE lt_mara_matgrp  INTO DATA(ls_mara_matgrp) INDEX 1  .
            IF sy-subrc EQ  0.
              " get the fert material to use further  for find characteristics
              lv_fert_matnr   = ls_mara_matgrp-matnr.
            ENDIF.
          ENDIF.
        ENDIF.
        CALL METHOD zcl_bom_explode_util=>bom_explode
          EXPORTING
            iv_matnr              = lv_fert_matnr
            iv_werks              = '4900'
*           ls_stpo               =
            iv_get_marterial_type = 'X'
*           iv_usage_probability  = 'X'
          CHANGING
            it_stpo               = lt_stpo.

        " get assembly characterostics
        DATA(lt_asy)   = lt_stpo   .
        DELETE lt_asy WHERE mtart NE 'ZASY'  .
        READ TABLE lt_asy  INTO DATA(ls_asy)  INDEX 1  .
        IF sy-subrc EQ 0.

          CALL METHOD zcl_zebr_form_dpc_ext=>get_mat_characteristics
            EXPORTING
              iv_matnr           = ls_asy-matnr
            IMPORTING
              et_allocvaluesnum  = lt_snum_asy
              et_allocvalueschar = lt_schar_asy
              et_allocvaluescurr = lt_scurr_asy
              et_return          = lt_return_asy.
          READ TABLE lt_schar_asy INTO DATA(ls_schar_asy)  WITH KEY charact = 'CARRIERTYPE' .
          IF sy-subrc EQ 0.
            ls_ebrheader-carr_chng_flag   = ls_schar_asy-value_char .
          ENDIF.


        ENDIF.

        " if  ebr is copied than fetch  all charasterics and  over write the above filled values
        IF lv_copied_flag  IS NOT INITIAL .
          CALL METHOD zcl_zebr_form_dpc_ext=>get_part_details
            EXPORTING
              iv_mfrpn     = ls_ebrheader-mfrpn
            CHANGING
              cs_ebrheader = ls_ebrheader.
        ENDIF.
        IF ls_header-lot_induct IS NOT INITIAL .
          ls_ebrheader-supply_status  = 'G'  .
        ENDIF.
        SELECT SINGLE bname , accnt   FROM usr02  INTO @DATA(ls_usr)  WHERE bname = @ls_header-created_by .
        IF ls_usr IS NOT INITIAL .
          SELECT  SINGLE employee_id , displayname , maximorg  FROM zworkday_data  INTO @DATA(ls_workday) WHERE  employee_id = @ls_usr-accnt .
          IF sy-subrc  EQ 0.
            ls_ebrheader-created_by   =  ls_workday-displayname .
            ls_ebrheader-group_ebr    = ls_workday-maximorg .
          ENDIF.
          CLEAR : ls_usr  ,  ls_workday .
        ENDIF .

        SELECT SINGLE bname  accnt   FROM usr02  INTO ls_usr  WHERE bname = ls_header-changed_by .
        IF ls_usr IS NOT INITIAL .
          SELECT  SINGLE employee_id  displayname  maximorg  FROM zworkday_data  INTO ls_workday WHERE  employee_id = ls_usr-accnt .
          IF sy-subrc  EQ 0.
            ls_ebrheader-changed_by   =  ls_workday-displayname .
          ENDIF.
          CLEAR : ls_usr  ,  ls_workday .
        ENDIF .

        SELECT SINGLE bname  accnt   FROM usr02  INTO ls_usr  WHERE bname = ls_header-npp_processor  .
        IF ls_usr IS NOT INITIAL .
          SELECT  SINGLE employee_id  displayname  maximorg  FROM zworkday_data  INTO ls_workday WHERE  employee_id = ls_usr-accnt .
          IF sy-subrc  EQ 0.
            ls_ebrheader-npp_name  =  ls_workday-displayname .
          ENDIF.
          CLEAR : ls_usr  ,  ls_workday .
        ENDIF .
        " check roles
        ls_ebrheader-display_edit   =  'D'  .                                           " default it as display
        IF ls_ebrheader-req_status_no  = '00'  OR  ls_ebrheader-req_status_no  = '01'  .
          CALL FUNCTION 'AUTHORITY_CHECK'
            EXPORTING
*             new_buffering       = 3
              user                = sy-uname
              object              = 'ZEBR_OBJ'
              field1              = 'ZEBR_ROLE'
              value1              = 'REQ'
            EXCEPTIONS
              user_dont_exist     = 1
              user_is_authorized  = 2
              user_not_authorized = 3
              user_is_locked      = 4
              OTHERS              = 5.
          IF sy-subrc EQ 0 OR sy-subrc EQ 2 .
            ls_ebrheader-display_edit  =  'E' .
            ls_ebrheader-discard_flag  =  'X' .
            ls_ebrheader-npp_auth      =  'X'  .
          ELSE.
            CALL FUNCTION 'AUTHORITY_CHECK'
              EXPORTING
*               new_buffering       = 3
                user                = sy-uname
                object              = 'ZEBR_OBJ'
                field1              = 'ZEBR_ROLE'
                value1              = 'SCM'
              EXCEPTIONS
                user_dont_exist     = 1
                user_is_authorized  = 2
                user_not_authorized = 3
                user_is_locked      = 4
                OTHERS              = 5.
            IF sy-subrc EQ 0 OR sy-subrc EQ 2 .
              " over write     to EDIT  if  user as reqyestor whenn status is draft(00) or submitted to NPP(01)
              ls_ebrheader-display_edit  =  'E' .
              ls_ebrheader-scm_flag      =  'X' .
              ls_ebrheader-discard_flag  =  'X' .
              ls_ebrheader-npp_auth      =  'X'  .
            ENDIF.
          ENDIF.
          " npp role
          CALL FUNCTION 'AUTHORITY_CHECK'
            EXPORTING
*             new_buffering       = 3
              user                = sy-uname
              object              = 'ZEBR_OBJ'
              field1              = 'ZEBR_ROLE'
              value1              = 'NPP'
            EXCEPTIONS
              user_dont_exist     = 1
              user_is_authorized  = 2
              user_not_authorized = 3
              user_is_locked      = 4
              OTHERS              = 5.
          IF sy-subrc EQ 0 OR sy-subrc EQ 2 .
            ls_ebrheader-npp_auth      =  'X'  .
          ENDIF.
        ELSEIF  ls_ebrheader-req_status_no  = '02'  OR  ls_ebrheader-req_status_no  = '98'  .
          "  for requestor in 02 status enable discard button
          CALL FUNCTION 'AUTHORITY_CHECK'
            EXPORTING
*             new_buffering       = 3
              user                = sy-uname
              object              = 'ZEBR_OBJ'
              field1              = 'ZEBR_ROLE'
              value1              = 'REQ'
            EXCEPTIONS
              user_dont_exist     = 1
              user_is_authorized  = 2
              user_not_authorized = 3
              user_is_locked      = 4
              OTHERS              = 5.
          IF sy-subrc EQ 0 OR sy-subrc EQ 2 .
            ls_ebrheader-discard_flag  =  'X' .
          ENDIF.
          " npp role
          CALL FUNCTION 'AUTHORITY_CHECK'
            EXPORTING
*             new_buffering       = 3
              user                = sy-uname
              object              = 'ZEBR_OBJ'
              field1              = 'ZEBR_ROLE'
              value1              = 'NPP'
            EXCEPTIONS
              user_dont_exist     = 1
              user_is_authorized  = 2
              user_not_authorized = 3
              user_is_locked      = 4
              OTHERS              = 5.
          IF sy-subrc EQ 0 OR sy-subrc EQ 2 .
            ls_ebrheader-display_edit =  'E' .
            ls_ebrheader-discard_flag  =  'X' .
            ls_ebrheader-npp_auth      =  'X'  .
          ELSE.
            " scm role
            CALL FUNCTION 'AUTHORITY_CHECK'
              EXPORTING
*               new_buffering       = 3
                user                = sy-uname
                object              = 'ZEBR_OBJ'
                field1              = 'ZEBR_ROLE'
                value1              = 'SCM'
              EXCEPTIONS
                user_dont_exist     = 1
                user_is_authorized  = 2
                user_not_authorized = 3
                user_is_locked      = 4
                OTHERS              = 5.
            IF sy-subrc EQ 0 OR sy-subrc EQ 2 .
              ls_ebrheader-display_edit  =  'E' .
              ls_ebrheader-scm_flag      =  'X' .                               " over write     to EDIT  if  user as reqyestor whenn status is draft(00) or submitted to NPP(01)
              ls_ebrheader-discard_flag  =  'X' .
              ls_ebrheader-npp_auth      =  'X'  .
            ENDIF.                                                              " over write     to EDIT  if  user as reqyestor whenn status is claim(02)
          ENDIF.
        ENDIF.

        CHECK NOT ls_ebrheader-req_no  IS INITIAL.
        lv_name = sy-mandt && ls_ebrheader-req_no .
        ls_ebrheader-part_desc =  read_text(
             EXPORTING
               id        = zcl_zebr_form_mpc_ext=>gc_tdid_part_des    " Text ID
               name      = lv_name    " Name
               object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

        ls_ebrheader-test_desc =  read_text(
             EXPORTING
               id        = zcl_zebr_form_mpc_ext=>gc_tdid_test_des    " Text ID
               name      = lv_name    " Name
               object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

        ls_ebrheader-assy_instr =  read_text(
            EXPORTING
              id        = zcl_zebr_form_mpc_ext=>gc_tdid_assy_instr    " Text ID
              name      = lv_name    " Name
              object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

        ls_ebrheader-bump_instr =  read_text(
             EXPORTING
               id        = zcl_zebr_form_mpc_ext=>gc_tdid_bump_instr    " Text ID
               name      = lv_name    " Name
               object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object
        ls_ebrheader-test_instr =  read_text(
           EXPORTING
             id        = zcl_zebr_form_mpc_ext=>gc_tdid_test_instr    " Text ID
             name      = lv_name    " Name
             object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

        ls_ebrheader-pbs_instr =  read_text(
             EXPORTING
               id        = zcl_zebr_form_mpc_ext=>gc_tdid_pbs_instr    " Text ID
               name      = lv_name    " Name
               object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

        ls_ebrheader-fert_instr =  read_text(
           EXPORTING
             id        = zcl_zebr_form_mpc_ext=>gc_tdid_fert_instr    " Text ID
             name      = lv_name    " Name
             object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

        ls_ebrheader-add_comment =  read_text(
           EXPORTING
             id        = zcl_zebr_form_mpc_ext=>gc_tdid_add_comment    " Text ID
             name      = lv_name    " Name
             object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object


        ls_ebrheader-npp_text =  read_text(
         EXPORTING
           id        = zcl_zebr_form_mpc_ext=>gc_tdid_npp_text    " Text ID
           name      = lv_name    " Name
           object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

        ls_ebrheader-sort_instr =  read_text(
           EXPORTING
             id        = zcl_zebr_form_mpc_ext=>gc_tdid_srt_instr    " Text ID
             name      = lv_name    " Name
             object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object


        ls_ebrheader-die_instr =  read_text(
           EXPORTING
             id        = zcl_zebr_form_mpc_ext=>gc_tdid_die_instr    " Text ID
             name      = lv_name    " Name
             object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object


        SELECT  * FROM  zebr_t_lotinfo INTO TABLE @DATA(lt_lotinfo)  WHERE req_no   IN @lrt_reqno .
        IF sy-subrc EQ 0.
          LOOP AT lt_lotinfo   INTO DATA(ls_lotinfo)  .
            APPEND INITIAL LINE TO ls_ebrheader-ebrheadlotnav   ASSIGNING FIELD-SYMBOL(<fs_ebrlotnav>)  .

            MOVE-CORRESPONDING  ls_lotinfo   TO <fs_ebrlotnav> .
            IF ls_ebrheader-mpw_wafer IS INITIAL .
              IF <fs_ebrlotnav>-sort_facility  IS NOT INITIAL .
                <fs_ebrlotnav>-sort_facility  =  <fs_ebrlotnav>-sort_plnnr && <fs_ebrlotnav>-sort_plnal .
              ENDIF .
              IF <fs_ebrlotnav>-pbs_facility IS NOT INITIAL .
                <fs_ebrlotnav>-pbs_facility  =  <fs_ebrlotnav>-pbs_plnnr && <fs_ebrlotnav>-pbs_plnal .
              ENDIF.

              IF <fs_ebrlotnav>-final_tst_facility  IS NOT INITIAL .
                <fs_ebrlotnav>-final_tst_facility  =  <fs_ebrlotnav>-final_tst_plnnr && <fs_ebrlotnav>-final_tst_plnal .

              ENDIF.

              IF <fs_ebrlotnav>-finish_facility IS NOT INITIAL .
                <fs_ebrlotnav>-finish_facility  =  <fs_ebrlotnav>-finish_plnnr && <fs_ebrlotnav>-finish_plnal .
              ENDIF.
            ENDIF.
            " assembly fac
            IF <fs_ebrlotnav>-asy_facility  IS NOT INITIAL AND <fs_ebrlotnav>-asy_facility NE 'ANY'  AND <fs_ebrlotnav>-asy_facility NE 'NEW'.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = <fs_ebrlotnav>-asy_facility
                IMPORTING
                  output = <fs_ebrlotnav>-asy_facility.

            ENDIF .
            " bump fac
            IF <fs_ebrlotnav>-bump_facility  IS NOT INITIAL AND <fs_ebrlotnav>-bump_facility NE 'ANY'  AND <fs_ebrlotnav>-bump_facility NE 'NEW'.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = <fs_ebrlotnav>-bump_facility
                IMPORTING
                  output = <fs_ebrlotnav>-bump_facility.

            ENDIF .

          ENDLOOP .
        ENDIF.

        " determine   first buy or not
        READ TABLE lt_lotinfo   INTO DATA(ls_headlotnav)  INDEX 1  .             "  to get assembly vendor  .
        IF sy-subrc EQ 0.
          DATA(lv_assy_fac)  =   ls_headlotnav-asy_facility.
          SHIFT lv_assy_fac LEFT DELETING LEADING '0'  .

          SELECT SINGLE  matnr , bismt ,
                         mtart , meins ,
                         mfrpn
            FROM mara
            INTO @DATA(ls_mara_bismt)
           WHERE mfrpn = @ls_header-mfrpn .

          SELECT SINGLE  expnr  FROM zcrossref_sto INTO  @DATA(lv_expnr) WHERE sursys = '5'
                                                                          AND flag   =  '3'     " flag value for assembly
                                                                          AND inpnr = @lv_assy_fac.
          IF sy-subrc EQ  0  .
            " old way of storing data in zscm_load_plan expnr
            SELECT SINGLE planning_id  INTO @DATA(lv_planid)  FROM zscm_load_plan   WHERE output_material  =  @ls_mara_bismt-bismt "#EC CI_NOFIELD
                                                                                     AND  asy_facility  =  @lv_expnr .
            IF sy-subrc EQ 0.
              ls_ebrheader-first_build  = 'Yes'.
            ELSE.
              SELECT SINGLE planning_id  INTO lv_planid  FROM zscm_load_plan   WHERE output_material  =  ls_header-mfrpn "#EC CI_NOFIELD
                                                                                       AND  asy_facility  =  lv_assy_fac.
              IF sy-subrc EQ 0.
                ls_ebrheader-first_build  = 'Yes'.
              ELSE.
                ls_ebrheader-first_build  = 'No'.
              ENDIF.
            ENDIF.
          ELSE.
            ls_ebrheader-first_build  = 'No'.
          ENDIF.
        ENDIF.



        SELECT * FROM zebr_t_notif  INTO TABLE @DATA(lt_notif)  WHERE req_no  IN @lrt_reqno .
        LOOP AT lt_notif INTO DATA(ls_ebrnotifnav)  .
          APPEND INITIAL LINE TO  ls_ebrheader-ebrnotifnav ASSIGNING FIELD-SYMBOL(<fs_ebrnotifnav>)  .
          MOVE-CORRESPONDING  ls_ebrnotifnav  TO <fs_ebrnotifnav>  .
        ENDLOOP.

        " for diaplying discard reason  text  in front used same field
        SELECT SINGLE field2 INTO @DATA(lv_discard_reason_text)  FROM zebr_config  WHERE  name = 'DISCARD_REASON'
                                                                                    AND   type  =  'DROP_DOWN'
                                                                                    AND   field1 = @ls_header-discard_reason .
        IF sy-subrc EQ 0 AND  lv_discard_reason_text   IS NOT INITIAL .
          ls_ebrheader-discard_reason  =    lv_discard_reason_text .
          CLEAR :  lv_discard_reason_text .
        ELSE.
          ls_ebrheader-discard_reason   =  ls_header-discard_reason.
        ENDIF.


      ENDIF.
      APPEND ls_ebrheader  TO  lt_ebrheader .
      copy_data_to_ref(
        EXPORTING
          is_data = lt_ebrheader
        CHANGING
          cr_data = er_entityset ).
    ENDIF.
  ENDMETHOD.


  METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM.
    DATA : ls_stream   TYPE ty_s_media_resource,
           ls_upld     TYPE zebr_t_file,
           lv_filename TYPE string,
           lv_req      TYPE zebr_de_req_no.
    DATA ls_lheader TYPE ihttpnvp.
    FIELD-SYMBOLS:<fs_key> TYPE /iwbep/s_mgw_name_value_pair.
    CLEAR ls_lheader.
    READ TABLE it_key_tab ASSIGNING <fs_key> INDEX 2.
    IF sy-subrc EQ 0.
      lv_filename = <fs_key>-value.
      CALL METHOD cl_http_utility=>unescape_url
          EXPORTING
            escaped   = lv_filename
          RECEIVING
            unescaped = lv_filename.
    ENDIF.
    READ TABLE it_key_tab ASSIGNING <fs_key> INDEX 1.
    IF sy-subrc EQ 0.
      lv_req = <fs_key>-value.
    ENDIF.
    SELECT SINGLE * FROM
      zebr_t_file INTO ls_upld WHERE req_no = lv_req AND filename = lv_filename.
    IF ls_upld IS NOT INITIAL.
      ls_lheader-name = 'Content-Disposition'.
      ls_lheader-value = 'outline; filename=' && lv_filename && ';'.
      set_header( is_header = ls_lheader ).
      ls_stream-value = ls_upld-value.
      ls_stream-mime_type = ls_upld-mimetype.
      copy_data_to_ref( EXPORTING is_data = ls_stream CHANGING cr_data = er_stream ).
    ENDIF.

  ENDMETHOD.


  METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~UPDATE_STREAM.
    DATA: lw_file TYPE zebr_t_file.
    FIELD-SYMBOLS:<fs_key> TYPE /iwbep/s_mgw_name_value_pair.
    READ TABLE it_key_tab ASSIGNING <fs_key> INDEX 1.
    lw_file-filename = <fs_key>-value.
    lw_file-value = is_media_resource-value.
    lw_file-mimetype = is_media_resource-mime_type.
    lw_file-sydate = sy-datum.
    lw_file-sytime = sy-uzeit.
    MODIFY zebr_t_file FROM lw_file.
  ENDMETHOD.


  METHOD BUILD_CODE.
    DATA  :   lv_package_enginer    TYPE boolean .
    CLEAR  :  lv_package_enginer .
    IF cs_ebrheader-req_no  ne  '*' .
      SELECT SINGLE created_by  FROM  zebr_t_header INTO @DATA(lv_uname)  WHERE req_no  = @cs_ebrheader-req_no .
    ELSE.
      lv_uname  =  sy-uname .
    ENDIF.
    SELECT SINGLE bname , accnt   FROM usr02  INTO @DATA(ls_usr)  WHERE bname = @lv_uname .
    IF ls_usr IS NOT INITIAL .
      SELECT * FROM zebr_config  INTO TABLE @DATA(lt_ebrconfig)  WHERE name  =  'PACKAGE_ENGINEER'  .
      SELECT   employee_id ,displayname , maximorg ,maximsuborg, maximfunction FROM zworkday_data  INTO TABLE  @DATA(lt_workday) FOR ALL ENTRIES IN  @lt_ebrconfig   "#EC CI_NO_TRANSFORM
                                                                                                      WHERE  employee_id    = @ls_usr-accnt
                                                                                                      AND    maximorg       = @lt_ebrconfig-field1+0(30)
                                                                                                      AND    maximsuborg    = @lt_ebrconfig-field2+0(30)
                                                                                                      AND    maximfunction  = @lt_ebrconfig-field4+0(30).
      IF sy-subrc EQ  0 .
        lv_package_enginer  = abap_true.
      ENDIF.
    ENDIF.
    IF cs_ebrheader-mpw_wafer EQ  'X'  .
      IF lv_package_enginer  IS NOT INITIAL .
        cs_ebrheader-bill_code = '/KE'  .
      ELSE.
        cs_ebrheader-bill_code = '/AE'  .
      ENDIF.
    ELSE.
      " release Type
      IF cs_ebrheader-req_type  =  'DOE'   OR
         cs_ebrheader-req_type  =  'NPI'   OR
         cs_ebrheader-req_type  =  'NP'    OR
         cs_ebrheader-req_type  =  'SLR'   OR
         cs_ebrheader-req_type  =  'TDE'   OR
         cs_ebrheader-req_type  =  'FEQ'   OR
         cs_ebrheader-req_type  =  'NP'  .
        IF lv_package_enginer  IS NOT INITIAL .
          IF cs_ebrheader-first_build EQ 'YES'  .
            cs_ebrheader-bill_code = '/KE'  .
          ELSE.
            IF cs_ebrheader-new_bom       EQ 'X'    OR
                cs_ebrheader-new_bnd       EQ 'X'    OR
                cs_ebrheader-doe           EQ 'X'    OR
                cs_ebrheader-att_map       EQ 'X'    OR
                cs_ebrheader-add_spec_top  EQ 'X'    .
              cs_ebrheader-bill_code = '/KE'  .
            ELSE.
              cs_ebrheader-bill_code = '/IE'  .
            ENDIF.
          ENDIF.
        ELSE.
          IF cs_ebrheader-first_build EQ 'YES'  .
            cs_ebrheader-bill_code = '/AE'  .
          ELSE.
            IF cs_ebrheader-new_bom       EQ 'X'    OR
                cs_ebrheader-new_bnd       EQ 'X'    OR
                cs_ebrheader-doe           EQ 'X'    OR
                cs_ebrheader-att_map       EQ 'X'    OR
                cs_ebrheader-add_spec_top  EQ 'X'    .
              cs_ebrheader-bill_code = '/AE'  .
            ELSE.
              cs_ebrheader-bill_code = '/IE'  .
            ENDIF.
          ENDIF.
        ENDIF.
      ELSEIF cs_ebrheader-req_type  =  'AECQ'   OR
             cs_ebrheader-req_type  =  'ENGR'   OR
             cs_ebrheader-req_type  =  'PKGQ'    OR
             cs_ebrheader-req_type  =  'NPI'   .

        IF lv_package_enginer  IS NOT INITIAL .
          cs_ebrheader-bill_code = '/KE'  .
        ELSE.
          cs_ebrheader-bill_code = '/AE'  .
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD EBRADDPARTSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRADDPARTSET_GET_ENTITYSET
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
    DATA : lv_string1         TYPE string,
           lv_string2         TYPE string,
           lrt_mfrpn          TYPE RANGE OF mfrpn,
           lrt_mfrpn_main     TYPE RANGE OF mfrpn,
           lt_stpo_main       TYPE ztt_bom_com_level,
           lt_stpo            TYPE ztt_bom_com_level,
           lt_allocvaluesnum  TYPE TABLE OF  bapi1003_alloc_values_num,
           lt_allocvalueschar TYPE  TABLE OF bapi1003_alloc_values_char,
           lt_allocvaluescurr TYPE  TABLE OF bapi1003_alloc_values_curr,
           lt_allocations     TYPE TABLE OF api_kssk,
           lv_classnum        TYPE  bapi1003_key-classnum,
           lv_classtype       TYPE  bapi1003_key-classtype,
           lt_return_t        TYPE TABLE OF bapiret2,
           lt_entityset       TYPE zcl_zebr_form_mpc=>tt_ebraddpart,
           lv_dietype         TYPE zcl_zebr_form_mpc_ext=>ts_ebraddpart-output_die_type.

    READ TABLE  it_filter_select_options  INTO DATA(ls_filter_mfrpn_tst)  WITH KEY  property  = 'Mfrpn'.
    IF sy-subrc EQ 0.
      DATA(lv_lines)  = lines( ls_filter_mfrpn_tst-select_options )  .
    ENDIF.
    DO lv_lines TIMES .
      DATA(lv_index)  = sy-index .
      LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options)  .

        IF ls_filter_select_options-property  = 'Mfrpn' .
          READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_mfrpn) INDEX  lv_index.
          IF sy-subrc EQ  0 .
            TRANSLATE  ls_filter_mfrpn-low  TO UPPER CASE .
            TRANSLATE  ls_filter_mfrpn-high  TO UPPER CASE .
            CALL METHOD cl_http_utility=>unescape_url
              EXPORTING
                escaped   = ls_filter_mfrpn-low
              RECEIVING
                unescaped = ls_filter_mfrpn-low.
            APPEND INITIAL LINE TO lrt_mfrpn ASSIGNING FIELD-SYMBOL(<fr_mfrpn>).
            <fr_mfrpn>-sign  =  ls_filter_mfrpn-sign .
            <fr_mfrpn>-option = ls_filter_mfrpn-option  .
            <fr_mfrpn>-low = ls_filter_mfrpn-low  .
            <fr_mfrpn>-high = ls_filter_mfrpn-high  .
          ENDIF.
        ELSEIF ls_filter_select_options-property  = 'MfrpnMain' .
          READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_mfrpn_main) INDEX lv_index .
          IF sy-subrc EQ  0 .
            TRANSLATE ls_filter_mfrpn_main-low TO UPPER CASE .
            TRANSLATE ls_filter_mfrpn_main-high TO UPPER CASE .
            CALL METHOD cl_http_utility=>unescape_url
              EXPORTING
                escaped   = ls_filter_mfrpn_main-low
              RECEIVING
                unescaped = ls_filter_mfrpn_main-low.
            APPEND INITIAL LINE TO lrt_mfrpn_main ASSIGNING FIELD-SYMBOL(<fr_mfrpn_main>).
            <fr_mfrpn_main>-sign  =  ls_filter_mfrpn_main-sign .
            <fr_mfrpn_main>-option = ls_filter_mfrpn_main-option  .
            <fr_mfrpn_main>-low = ls_filter_mfrpn_main-low  .
            <fr_mfrpn_main>-high = ls_filter_mfrpn_main-high  .
          ENDIF.

        ELSEIF ls_filter_select_options-property  = 'OutputDieType' .
          READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_die_type) INDEX lv_index .
          IF sy-subrc EQ  0   .
            IF  ls_filter_die_type-low   IS  NOT INITIAL .
              lv_dietype  =    ls_filter_die_type-low  .
            ENDIF.
          ENDIF.

        ENDIF.
      ENDLOOP.

      " phantom die
      SELECT SINGLE matnr ,mfrpn   FROM   mara INTO @DATA(ls_mara_main)   WHERE mfrpn  IN @lrt_mfrpn_main .


      SELECT SINGLE matnr ,mfrpn FROM mara INTO @DATA(ls_mara)  WHERE mfrpn IN @lrt_mfrpn .
      " get bom numberf rom mast table basen on  material number form screen
      CALL METHOD zcl_bom_explode_util=>bom_explode
        EXPORTING
          iv_matnr              = ls_mara-matnr
          iv_werks              = '4900'
*         ls_stpo               =
          iv_get_marterial_type = 'X'
*          iv_usage_probability  = 'X'
        CHANGING
          it_stpo               = lt_stpo.
      DATA(lt_stpo_phantom)   = lt_stpo .
      " get all fab materials to find  die type and die rev
      DELETE lt_stpo WHERE mtart  NE 'ZFAB'  .

      " phantom die logic
      LOOP AT lt_stpo_phantom INTO DATA(ls_stpo_main)  .
        IF  ls_stpo_main-zdie_type   IS NOT INITIAL  AND ls_stpo_main-zdie_revcode   IS NOT INITIAL  .
          IF lv_dietype  IS INITIAL  .
            APPEND INITIAL LINE TO lt_entityset ASSIGNING FIELD-SYMBOL(<fs_addpart>)  .
            <fs_addpart>-mfrpn        = ls_mara-mfrpn .
            <fs_addpart>-mfrpn_main   = ls_mara_main-mfrpn .
            <fs_addpart>-output_part  = ls_mara-matnr .

            <fs_addpart>-output_die_type  =  ls_stpo_main-zdie_type .

*        <fs_addpart>-output_die_rev   =  ls_stpo_main-zdie_revcode .
          ELSEIF  lv_dietype IS NOT INITIAL  AND lv_dietype   EQ ls_stpo_main-zdie_type  .
            APPEND INITIAL LINE TO lt_entityset ASSIGNING <fs_addpart> .
            <fs_addpart>-mfrpn        = ls_mara-mfrpn .
            <fs_addpart>-mfrpn_main   = ls_mara_main-mfrpn .
            <fs_addpart>-output_part  = ls_mara-matnr .
            <fs_addpart>-output_die_type  =  ls_stpo_main-zdie_type .
            <fs_addpart>-output_die_rev   =  ls_stpo_main-zdie_revcode .

          ENDIF.
        ENDIF.
        CLEAR ls_stpo_main .
      ENDLOOP .
      IF lt_entityset  IS  INITIAL .   " skip all fabs when we find phantom  die i.e,  its  already filled a step up
        LOOP AT lt_stpo INTO DATA(ls_stpo)  .
          " get charact  of all materils if owner code is 'I'  dont show .
          " owner code
          CALL METHOD zcl_zebr_form_dpc_ext=>get_mat_characteristics
            EXPORTING
              iv_matnr           = ls_stpo-matnr
            IMPORTING
              et_allocvaluesnum  = lt_allocvaluesnum
              et_allocvalueschar = lt_allocvalueschar
              et_allocvaluescurr = lt_allocvaluescurr
              et_return          = lt_return_t.
          READ TABLE lt_allocvalueschar INTO DATA(ls_char) WITH KEY  charact  = 'OWNER_CODE' .
          IF sy-subrc EQ 0 AND ls_char-value_char  EQ 'I' .
            " skip
          ELSE.
            APPEND INITIAL LINE TO lt_entityset ASSIGNING <fs_addpart>  .
            <fs_addpart>-mfrpn        = ls_mara-mfrpn .
            <fs_addpart>-mfrpn_main   = ls_mara_main-mfrpn .
            <fs_addpart>-output_part  = ls_mara-matnr .

            DATA(lv_temp)             = ls_stpo-matnr .
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
              EXPORTING
                input  = lv_temp
              IMPORTING
                output = lv_temp.

            " split  MAX-123-234  .  split at second  '-'  .
            DO .
              IF lv_temp  CA  '-' .
                SPLIT lv_temp AT '-'  INTO lv_string1  lv_string2  .
                IF <fs_addpart>-output_die_type IS INITIAL .
                  <fs_addpart>-output_die_type  =  lv_string1 .
                  <fs_addpart>-output_die_rev   =  lv_string2 .
                ELSE.
                  <fs_addpart>-output_die_type  =   <fs_addpart>-output_die_type && '-' && lv_string1  .
                  <fs_addpart>-output_die_rev   =  lv_string2 .
                ENDIF.
                lv_temp   = lv_string2  .
                CLEAR :  lv_string1  , lv_string2 .
              ELSE.
                EXIT.
              ENDIF.
            ENDDO.
            " die type will be cleared
            IF lv_dietype IS INITIAL  .
              CLEAR <fs_addpart>-output_die_rev  .
            ENDIF  .

          ENDIF.
          REFRESH : lt_allocvaluesnum ,
                    lt_allocvalueschar ,
                    lt_allocvaluescurr ,
                    lt_return_t.

        ENDLOOP .
      ENDIF.



      SORT lt_entityset  BY mfrpn_main mfrpn  output_die_type  output_die_rev .
      DELETE ADJACENT DUPLICATES FROM  lt_entityset  COMPARING  mfrpn_main mfrpn  output_die_type  output_die_rev  .

      IF lv_dietype IS NOT INITIAL  .
        LOOP AT lt_entityset  ASSIGNING FIELD-SYMBOL(<fs_entity>) WHERE output_die_type  NE  lv_dietype  .
          CLEAR <fs_entity>-output_die_rev   .
        ENDLOOP.
      ENDIF.

      IF ls_mara-mfrpn IS NOT INITIAL .
        APPEND INITIAL LINE TO lt_entityset ASSIGNING <fs_addpart>  .
        <fs_addpart>-mfrpn        = ls_mara-mfrpn .
        <fs_addpart>-mfrpn_main   = ls_mara_main-mfrpn .
        <fs_addpart>-output_part  = ls_mara-matnr .
        <fs_addpart>-output_die_type  =  'NEW' .
      ENDIF.

      APPEND LINES OF  lt_entityset TO et_entityset  .
      CLEAR  :  lrt_mfrpn[]       ,
                lrt_mfrpn_main[]  ,
                lt_stpo_main[]    ,
                lt_stpo[]           ,
                lt_allocvaluesnum[] ,
                lt_allocvalueschar[] ,
                lt_allocvaluescurr[] ,
                lt_allocations[]     ,
                lv_classnum    ,
                lv_classtype    ,
                lt_return_t[] ,
                lv_string1,
                lv_string2,
                lv_index,
                lt_entityset[],
                lv_dietype                     .
    ENDDO.



  ENDMETHOD.


  method EBRASSYFACSET_GET_ENTITYSET.
*    **TRY.
*CALL METHOD SUPER->EBRASSYFACSET_GET_ENTITYSET
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
    data : lt_fac  type zcl_zebr_form_mpc_ext=>tt_ebrassyfac   .
    APPEND INITIAL LINE TO lt_fac ASSIGNING FIELD-SYMBOL(<fs_fac>)  .
    <fs_fac>-mfrpn  = ' '   .
    et_entityset  = lt_fac .

  endmethod.


  method EBRBILLCODESET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->EBRBILLCODESET_GET_ENTITY
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
  endmethod.


  method EBRBILLCODESET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRBILLCODESET_GET_ENTITYSET
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

    SELECT * from ZEBR_CONFIG  into table @data(lt_zebr_config)  where name  = 'BILLCODE'                    "#EC CI_ALL_FIELDS_NEEDED
                                                                  and type = 'DROP_DOWN'  .

   loop at lt_zebr_config into data(ls_zebr_config)  .
     APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entityset>)  .
     <fs_entityset>-bill_code  = ls_zebr_config-field1  .
     <fs_entityset>-bill_code_desc  = ls_zebr_config-field2 .
   ENDLOOP.
  endmethod.


  METHOD EBRBNDNOSET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->EBRBNDNOSET_GET_ENTITY
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
    DATA : lv_mfrpn        TYPE mara-mfrpn,
           lv_licha        TYPE mch1-licha,
           lv_lifnr        TYPE  lifnr,
           lt_bom_details  TYPE ztt_bom_item,
           lt_bom_ip_out   TYPE ztt_bom_ip_out,
           lt_stpo_bom     TYPE ztt_bom_com_level,
           lt_stpo_bom_asy TYPE ztt_bom_com_level.
    READ TABLE it_key_tab INTO DATA(ls_key_tab)   WITH KEY   name  = 'Mfrpn' .
    IF sy-subrc EQ  0 .
      "  getting  encoded mfrpn  incosistently form frontend  .
      SELECT SINGLE  matnr  ,mfrpn INTO @DATA(ls_mfrpn_valid) FROM mara  WHERE mfrpn  = @ls_key_tab-value   .
      IF sy-subrc EQ   0 .
        lv_mfrpn   =  ls_key_tab-value .
      ELSE.
        CALL METHOD cl_http_utility=>unescape_url
          EXPORTING
            escaped   = ls_key_tab-value
          RECEIVING
            unescaped = ls_key_tab-value.


        lv_mfrpn   =  ls_key_tab-value .
      ENDIF.



      READ TABLE it_key_tab  INTO ls_key_tab WITH KEY name   =  'LotNo' .
      IF sy-subrc EQ 0.
        lv_licha  =  ls_key_tab-value .
      ENDIF.

      READ TABLE it_key_tab  INTO ls_key_tab  WITH KEY name   =  'AsyFacility' .
      IF sy-subrc EQ 0.
        lv_lifnr  =  ls_key_tab-value .
      ENDIF.
      er_entity-mfrpn         =  lv_mfrpn.
      er_entity-lot_no        =  lv_licha  .
      er_entity-asy_facility  =  lv_licha  .

      SELECT SINGLE matnr  , mfrpn  FROM  mara INTO @DATA(ls_mara) WHERE  mfrpn =  @lv_mfrpn .
      IF sy-subrc EQ 0.
        CALL METHOD zcl_bom_explode_util=>bom_explode
          EXPORTING
            iv_matnr              = ls_mara-matnr
            iv_werks              = '4900'
*           ls_stpo               =
            iv_get_marterial_type = 'X'
*            iv_usage_probability  = 'X'
          CHANGING
            it_stpo               = lt_stpo_bom.
      ENDIF.
      "  get stpo bom
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_lifnr
        IMPORTING
          output = lv_lifnr.
      .

      READ TABLE lt_stpo_bom INTO DATA(ls_stpo_bom)    WITH KEY lifnr  =  lv_lifnr  .
      IF sy-subrc EQ  0.
        "  get bond diagram number for assembly materials  if it is not found
        " then find it's sort material
        SELECT SINGLE a~doknr , b~dokvr  INTO @DATA(ls_bnd)
                 FROM drad AS a INNER JOIN  draw AS b  ON  a~dokar  =  b~dokar
                                                       AND a~doknr  =  b~doknr
                                                     WHERE a~dokar  =  'BND'
                                                       AND a~objky  =  @ls_stpo_bom-item_guidx
                                                       AND b~dokst  =  'RL' . "#EC *  " bond diagram fetching based on guid
        IF sy-subrc EQ 0.
          er_entity-bnd_number    =  ls_bnd-doknr && '-' && ls_bnd-dokvr  .
        ELSE .
          "  find material & material type  based on lot
          SELECT   a~matnr , a~mtart , b~licha  INTO  TABLE @DATA(lt_mch1)
                                  FROM  mara AS a INNER JOIN   mch1   AS b
                                         ON a~matnr   = b~matnr
                                         WHERE licha  = @lv_licha
                                          AND a~mtart   IN ('ZFAB','ZSRT') .
          IF sy-subrc EQ 0  .
            "  find sort materil
            READ TABLE  lt_mch1  INTO DATA(ls_mch1)  WITH KEY mtart  =  'ZSRT'
                                                              licha  = lv_licha .
            IF sy-subrc  EQ 0.  .
              READ TABLE lt_stpo_bom INTO DATA(ls_stpo_srt)    WITH KEY matnr  =  ls_mch1-matnr  .
              IF sy-subrc EQ 0.
                CLEAR  ls_bnd  .
                SELECT SINGLE a~doknr  b~dokvr  INTO ls_bnd
                FROM drad AS a INNER JOIN  draw AS b  ON  a~dokar  =  b~dokar
                                                      AND a~doknr  =  b~doknr
                                                    WHERE a~dokar  =  'BND'
                                                      AND a~objky  =  ls_stpo_srt-item_guidx
                                                      AND b~dokst  =  'RL' . "#EC *  " bond diagram fetching based on guid
                IF sy-subrc EQ 0.
                  er_entity-bnd_number    =  ls_bnd-doknr && '/' && ls_bnd-dokvr  .
                ENDIF.
              ENDIF.
              " find fab materila if zsrt material is not found
            ELSE.
              READ TABLE  lt_mch1  INTO ls_mch1  WITH KEY mtart  =  'ZFAB'
                                                              licha  = lv_licha .
              IF sy-subrc EQ 0.
                " explode   assembly bom with alternate(is detemeined base on vendor)
                CALL METHOD zcl_bom_explode_util=>bom_explode
                  EXPORTING
                    iv_matnr              = ls_stpo_bom-matnr
                    iv_werks              = '4900'
*                   ls_stpo               =
                    iv_get_marterial_type = 'X'
                    iv_bom_alt            = ls_stpo_bom-alt
                    iv_active_bom_01      = 'X'
                  CHANGING
                    it_stpo               = lt_stpo_bom_asy.

                " identfity  fab material  level in bom
                " a level up in  bom above fab material sort will be available(remove last two character in zfab material  we wil get sort material)
                " example
                "  1.1.3      MAX2132_S1     ZSRT      (one level up zsrt mateil)
                "  1.1.3.1    MAX2132        ZUNB
                "  1.1.3.2   WYSB_YB_v14u    ZFAB      (fab material)
                READ TABLE   lt_stpo_bom_asy INTO DATA(ls_stpo_bom_fab)  WITH KEY  matnr  = ls_mch1-matnr .
                IF sy-subrc EQ  0.
                  DATA(lv_len) = strlen( ls_stpo_bom_fab-level )   .
                  lv_len  = lv_len  - 2 .

                  CLEAR ls_stpo_srt.
                  READ TABLE lt_stpo_bom_asy INTO ls_stpo_srt  WITH KEY level  = ls_stpo_bom_fab-level+0(lv_len)  .
                  IF sy-subrc EQ 0.
                    CLEAR  ls_bnd  .
                    SELECT SINGLE a~doknr  b~dokvr  INTO ls_bnd
                    FROM drad AS a INNER JOIN  draw AS b  ON  a~dokar  =  b~dokar
                                                          AND a~doknr  =  b~doknr
                                                        WHERE a~dokar  =  'BND'
                                                          AND a~objky  =  ls_stpo_srt-item_guidx
                                                          AND b~dokst  =  'RL'  . "#EC *  " bond diagram fetching based on guid
                    IF sy-subrc EQ 0.
                      er_entity-bnd_number    =  ls_bnd-doknr && '/' && ls_bnd-dokvr  .
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF .
    ENDIF.



  ENDMETHOD.


  METHOD EBRBUMPFACSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRBUMPFACSET_GET_ENTITYSET
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
    DATA : lt_fac  TYPE zcl_zebr_form_mpc_ext=>tt_ebrpbsfac   .
        APPEND INITIAL LINE TO lt_fac ASSIGNING FIELD-SYMBOL(<fs_fac>)  .
    <fs_fac>-mfrpn  = ' '   .

    et_entityset  = lt_fac .

  ENDMETHOD.


  METHOD EBRCARRIERTYPESE_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRCARRIERTYPESE_GET_ENTITYSET
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
    DATA: lt_messages TYPE TABLE OF bapiret2,
          lt_num      TYPE tt_bapicharactvaluesnum,
          lt_values	  TYPE tt_bapicharactvaluesdescr.

    CLEAR lt_messages[].

    CALL FUNCTION 'BAPI_CHARACT_GETDETAIL'
      EXPORTING
        charactname        = 'CARRIERTYPE'
      TABLES
        charactvaluesdescr = lt_values
        charactvaluesnum   = lt_num
        return             = lt_messages.
    LOOP AT lt_values  INTO  DATA(ls_values) .
      APPEND INITIAL LINE TO  et_entityset  ASSIGNING FIELD-SYMBOL(<fs_entityset>) .
      <fs_entityset>-carrier_type  = ls_values-description  .
    ENDLOOP.
    ENDMETHOD.


  METHOD EBRCHATSET_CREATE_ENTITY.
**TRY.
*CALL METHOD SUPER->EBRCHATSET_CREATE_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
**    io_data_provider        =
**  IMPORTING
**    er_entity               =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    DATA : ls_ebrchat     TYPE zcl_zebr_form_mpc=>ts_ebrchat,
           lv_date        TYPE char10,
           lv_time        TYPE char08,
           lv_displayname TYPE char100,
           lv_name        TYPE thead-tdname,
           lv_chat        TYPE  string.



    " get data from screen
    io_data_provider->read_entry_data(
   IMPORTING
   es_data = ls_ebrchat  ).

    lv_name = sy-mandt && ls_ebrchat-req_no.

    SELECT SINGLE bname , accnt   FROM usr02  INTO @DATA(ls_usr)  WHERE bname = @sy-uname .
    IF ls_usr IS NOT INITIAL .
      SELECT  SINGLE employee_id ,displayname , maximorg  FROM zworkday_data  INTO @DATA(ls_workday) WHERE  employee_id = @ls_usr-accnt .
    ENDIF.

    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
      EXPORTING
        date_internal            = sy-datum
      IMPORTING
        date_external            = lv_date
      EXCEPTIONS
        date_internal_is_invalid = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_BCVTM_OUTPUT'
      EXPORTING
        input  = sy-uzeit
      IMPORTING
        output = lv_time.


    CONCATENATE ls_workday-displayname  '('  lv_date  ' '  lv_time  ')'  cl_abap_char_utilities=>newline
                 ls_ebrchat-chat   INTO   ls_ebrchat-chat  .

    CLEAR  lv_chat .
    save_text_chat(
      EXPORTING
        id        = zcl_zebr_form_mpc_ext=>gc_tdid_chat    " Text ID
        name      = lv_name                                " Name
        object    = zcl_zebr_form_mpc_ext=>gc_object       " Texts: application object
        text      =  ls_ebrchat-chat
      IMPORTING
        chat_text =   lv_chat ).

*    "  disabling    email notification  for  live chat
*    " send mail
*    TRY.
*        CALL METHOD zcl_ebr_utility=>email_functionality
*          EXPORTING
*            im_ebr      = ls_ebrchat-req_no
*            im_req_type = 'N'
**           im_req_status = is_ebrheader-req_status_no
*            im_group    = zcl_ebr_utility=>zemail_grp_live_chat
**            IMPORTING
**           et_notif    =
*          .
*      CATCH cx_abap_invalid_value .
*    ENDTRY.

    ls_ebrchat-chat   = lv_chat .

    er_entity  = ls_ebrchat .
*    " send data  to deep entity
*    copy_data_to_ref(
*         EXPORTING
*         is_data = ls_ebrchat
*         CHANGING
*         cr_data = ER_ENTITY ).


  ENDMETHOD.


  METHOD EBRCHATSET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->EBRCHATSET_GET_ENTITY
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
    data : lv_name type thead-tdname.
    READ TABLE it_key_tab INTO DATA(ls_key_tab)  WITH  KEY name = 'ReqNo'  .
    IF sy-subrc EQ 0 .

      SELECT SINGLE req_no  INTO @DATA(lv_req_no)
             FROM zebr_t_header WHERE req_no = @ls_key_tab-value .
      IF sy-subrc EQ 0.
        lv_name = sy-mandt && lv_req_no .
        er_entity-req_no  = lv_req_no .
        er_entity-chat =  read_text(
           EXPORTING
             id        = zcl_zebr_form_mpc_ext=>GC_TDID_CHAT    " Text ID
             name      = lv_name    " Name
             object    = zcl_zebr_form_mpc_ext=>gc_object ).

      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD EBRDCFACSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRDCFACSET_GET_ENTITYSET
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

    TYPES: ts_stpo_level  TYPE  zbom_com_level,
           tty_stpo_level TYPE  ztt_bom_com_level.
    DATA : lv_mfrpn      TYPE mara-mfrpn,
           lt_stpo       TYPE  tty_stpo_level,
           lv_matnr_op   TYPE matnr,
           lrt_mfrpn     TYPE RANGE OF mfrpn,
           lt_fac        TYPE zcl_zebr_form_mpc_ext=>tt_ebrgetfacility,
           lv_asy_index  TYPE i,
           lv_bump_index TYPE i,
           lv_pbs_index  TYPE i,
           lv_fert_index TYPE i,
           lv_test_index TYPE i,
           lv_index      TYPE  i,
           lv_dc_index   TYPE i,
           lt_stpo_route TYPE  ztt_bom_com_level,
           ls_fac        TYPE zcl_zebr_form_mpc_ext=>ts_ebrgetfacility,
           lrt_mtart     TYPE RANGE OF  mtart,
           lv_date       TYPE rc29l-datub,
           lv_datuv      TYPE rc29l-datuv,
           lt_stpov      TYPE STANDARD TABLE OF stpov,
           lt_equicat    TYPE TABLE OF cscequi,
           lt_kndcat     TYPE TABLE OF cscknd,
           lt_matcat     TYPE TABLE OF cscmat,
           lt_stdcat     TYPE TABLE OF cscstd,
           lt_tplcat     TYPE TABLE OF csctpl,
           lt_prjcat     TYPE TABLE OF cscprj,
           lrt_licha     TYPE RANGE OF lichn.


    APPEND INITIAL LINE TO  lrt_mtart ASSIGNING FIELD-SYMBOL(<fr_mtart>).
    <fr_mtart>-sign   = 'I' .
    <fr_mtart>-option = 'EQ'  .
    <fr_mtart>-low    =  'ZFAB'  .


    APPEND INITIAL LINE TO  lrt_mtart ASSIGNING <fr_mtart>.
    <fr_mtart>-sign   = 'I' .
    <fr_mtart>-option = 'EQ'  .
    <fr_mtart>-low    =  'ZSRT'  .

*Begin of change JGAND37207
    APPEND INITIAL LINE TO  lrt_mtart ASSIGNING <fr_mtart>.
    <fr_mtart>-sign   = 'I' .
    <fr_mtart>-option = 'EQ'  .
    <fr_mtart>-low    =  'ZUNB'  .
*End of change JGAND37207

    LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options)  .
      IF ls_filter_select_options-property  = 'Mfrpn'  .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_mfrpn)  INDEX 1.
        IF sy-subrc EQ  0 .
          IF ls_filter_mfrpn-low  IS NOT INITIAL .
            CALL METHOD cl_http_utility=>unescape_url
              EXPORTING
                escaped   = ls_filter_mfrpn-low
              RECEIVING
                unescaped = ls_filter_mfrpn-low.
            TRANSLATE ls_filter_mfrpn-low TO UPPER CASE .
            TRANSLATE ls_filter_mfrpn-high TO UPPER CASE .
            APPEND INITIAL LINE TO lrt_mfrpn ASSIGNING FIELD-SYMBOL(<fr_mfrpn>).
            <fr_mfrpn>-sign   = ls_filter_mfrpn-sign .
            <fr_mfrpn>-option = ls_filter_mfrpn-option  .
            <fr_mfrpn>-low    = ls_filter_mfrpn-low  .
            <fr_mfrpn>-high   = ls_filter_mfrpn-high  .
          ENDIF.
        ENDIF.
      ENDIF.

      IF ls_filter_select_options-property  = 'LotNo'  .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_lotno)  INDEX 1.
        IF sy-subrc EQ  0 AND  ls_filter_lotno-low  IS NOT INITIAL.

          TRANSLATE ls_filter_lotno-low TO UPPER CASE .
          TRANSLATE ls_filter_lotno-high TO UPPER CASE .
          APPEND INITIAL LINE TO lrt_licha ASSIGNING FIELD-SYMBOL(<fr_licha>).
          <fr_licha>-sign   = ls_filter_lotno-sign .
          <fr_licha>-option = ls_filter_lotno-option  .
          <fr_licha>-low    = ls_filter_lotno-low  .
          <fr_licha>-high   = ls_filter_lotno-high  .
        ENDIF.
      ENDIF.
    ENDLOOP.



    SELECT SINGLE  matnr, mfrpn FROM  mara INTO @DATA(ls_mara)  WHERE mfrpn  IN @lrt_mfrpn.
    " get bom numberf rom mast table basen on  material number form screen
    CALL METHOD zcl_bom_explode_util=>bom_explode
      EXPORTING
        iv_matnr              = ls_mara-matnr
        iv_werks              = '4900'
*       ls_stpo               =
        iv_get_marterial_type = 'X'
*        iv_usage_probability  = 'X'
      CHANGING
        it_stpo               = lt_stpo.

    DATA(lt_stpo_mat)  = lt_stpo .
    DELETE lt_stpo_mat  WHERE  mtart NOT IN lrt_mtart .



    " get die coat vendors
    IF lt_stpo IS NOT INITIAL  .
      " get lot details
      SELECT a~matnr , a~mtart, a~groes , a~mfrpn , b~charg , b~licha  ,c~werks , c~lgort   , c~clabs  ,c~ceinm
             INTO TABLE  @DATA(lt_mara_mch1) FROM  mara  AS a  INNER JOIN mch1  AS b
                                                  ON  a~matnr   = b~matnr
                                                  INNER JOIN mchb AS c
                                                  ON b~matnr  = c~matnr
                                                 AND b~charg  = c~charg
                                                 INNER JOIN t320 AS d
                                                  ON    c~werks  =  d~werks
                                                  AND   c~lgort  =  d~lgort
                                     FOR ALL ENTRIES IN @lt_stpo_mat
                                                  WHERE a~matnr  EQ @lt_stpo_mat-matnr
                                                   AND  a~mtart  IN @lrt_mtart
                                                   AND  b~licha  IN  @lrt_licha
                                                   AND ( c~clabs    GT  0 OR c~ceinm GT  0 ).
      IF sy-subrc EQ 0.
        " assumbing vendor lot with quantity will be avaialble only  either one of  zfab or zsrt
        READ TABLE  lt_mara_mch1 INTO DATA(ls_mara_mch1)  INDEX 1 .
        IF sy-subrc EQ 0.
          IF ls_mara_mch1-mtart  = 'ZSRT'  .
            READ TABLE lt_stpo INTO DATA(ls_stpo)  WITH KEY matnr  = ls_mara_mch1-matnr .
            IF sy-subrc EQ 0.
              DATA(lv_sortf)   =  ls_stpo-sortf .
            ENDIF.
          ELSEIF ls_mara_mch1-mtart  = 'ZFAB'.
            lv_date  = sy-datum .
            lv_datuv = sy-datum .
            CALL FUNCTION 'CS_WHERE_USED_MAT'
              EXPORTING
                datub                      = lv_date
                datuv                      = lv_datuv
                matnr                      = ls_mara_mch1-matnr
                stlan                      = '1'
                stltp                      = 'M'
              TABLES
                wultb                      = lt_stpov
                equicat                    = lt_equicat
                kndcat                     = lt_kndcat
                matcat                     = lt_matcat
                stdcat                     = lt_stdcat
                tplcat                     = lt_tplcat
                prjcat                     = lt_prjcat
              EXCEPTIONS
*               CALL_INVALID               = 01
                material_not_found         = 02
                no_where_used_rec_found    = 03
                no_where_used_rec_selected = 04
                no_where_used_rec_valid    = 05.
            IF sy-subrc NE 0 .
              REFRESH : lt_equicat[],
                   lt_kndcat[],
                   lt_matcat[],
                   lt_stdcat[],
                   lt_tplcat[],
                   lt_prjcat[].
              EXIT.
            ELSE.
              REFRESH : lt_equicat[],
                        lt_kndcat[],
                        lt_matcat[],
                        lt_stdcat[],
                        lt_tplcat[],
                        lt_prjcat[].
              DELETE lt_stpov WHERE bmtyp NE 'M' .
              SORT lt_stpov BY matnr bmtyp ttidx .
              DELETE ADJACENT DUPLICATES FROM lt_stpov COMPARING matnr bmtyp ttidx idnrk .
              " fab to sort will be always one is  one
              READ TABLE lt_stpov INTO DATA(ls_stpov)   INDEX 1 .
              IF sy-subrc EQ 0.
                CLEAR ls_stpo.
                READ TABLE lt_stpo INTO ls_stpo  WITH KEY matnr  = ls_stpov-matnr .
                IF sy-subrc EQ 0.
                  CLEAR lv_sortf .
                  lv_sortf   =  ls_stpo-sortf .
                ENDIF.
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.



        SELECT a~matnr , a~matkl , b~werks , b~lifnr   INTO TABLE @DATA(lt_diecoat)  FROM mara AS a INNER JOIN eord AS b "#EC CI_NO_TRANSFORM
                                                            ON a~matnr   = b~matnr
                                                         FOR ALL ENTRIES IN @lt_stpo
                                                        WHERE a~matnr = @lt_stpo-matnr
                                                         AND  a~matkl  IN ('119' ,'132'  ) .
        IF sy-subrc EQ 0.
          SELECT  lifnr , name1  FROM lfa1  INTO  TABLE @DATA(lt_lfa1)
            FOR ALL ENTRIES IN  @lt_diecoat  WHERE lifnr   =  @lt_diecoat-lifnr . "#EC CI_NO_TRANSFORM
          IF sy-subrc  EQ 0 .
*          SORT lt_diecoat  BY lifnr  .
*          DELETE ADJACENT DUPLICATES FROM  lt_diecoat COMPARING lifnr  .
            LOOP AT lt_diecoat  INTO DATA(ls_diecoat)   .
              READ TABLE  lt_stpo INTO ls_stpo  WITH KEY matnr  = ls_diecoat-matnr .
              IF sy-subrc EQ 0    AND ( ( ls_stpo-sortf  = lv_sortf )  OR ls_stpo-sortf IS INITIAL ).
                APPEND INITIAL LINE TO et_entityset  ASSIGNING FIELD-SYMBOL(<fs_diecoat>)  .
                <fs_diecoat>-die_coat  =  ls_diecoat-lifnr .
                READ TABLE lt_lfa1 INTO DATA(ls_lfa1)  WITH KEY   lifnr  = ls_diecoat-lifnr .
                IF sy-subrc EQ 0.
                  <fs_diecoat>-dc_facdesc  =  ls_lfa1-name1 .
                ENDIF .
              ENDIF.
            ENDLOOP .
          ENDIF.
        ENDIF .
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD EBRDIEQTYSET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->EBRDIEQTYSET_GET_ENTITY
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
    DATA  : lt_stpo_bom_fab    TYPE ztt_bom_com_level,
            lt_allocvaluesnum  TYPE TABLE OF  bapi1003_alloc_values_num,
            lt_allocvalueschar TYPE  TABLE OF bapi1003_alloc_values_char,
            lt_allocvaluescurr TYPE  TABLE OF bapi1003_alloc_values_curr,
            lt_allocations     TYPE TABLE OF api_kssk,
            lt_return          TYPE TABLE OF bapiret2.
    READ  TABLE   it_key_tab  INTO DATA(ls_key_tab_lot)   WITH KEY name  =  'LotNo'  .
    IF sy-subrc EQ 0.
      er_entity-lot_no  = ls_key_tab_lot-value  .
    ENDIF.

    READ  TABLE   it_key_tab  INTO DATA(ls_key_tab_dtype)   WITH KEY name  =  'LotDieType'  .
    IF sy-subrc EQ 0.
      er_entity-lot_die_type  = ls_key_tab_dtype-value  .
    ENDIF.

    READ  TABLE   it_key_tab  INTO DATA(ls_key_tab_drev)   WITH KEY name  =  'LotDieRev'  .
    IF sy-subrc EQ 0.
      er_entity-lot_die_rev  = ls_key_tab_drev-value  .
    ENDIF.

    SELECT  a~matnr, a~charg  ,a~licha , b~mtart INTO TABLE @DATA(lt_mch1)
                                          FROM   mch1  AS a INNER JOIN mara   AS b
                                          ON a~matnr  = b~matnr
                                          WHERE a~licha   EQ  @er_entity-lot_no
                                          AND   b~mtart    IN ('ZSRT','ZFAB') .
    READ TABLE  lt_mch1 INTO DATA(ls_mch1_gdpw)   WITH KEY licha  = er_entity-lot_no
                                                            mtart  = 'ZSRT'.
    IF sy-subrc EQ 0  .
      "  zfab material
      CALL METHOD zcl_bom_explode_util=>bom_explode
        EXPORTING
          iv_matnr              = ls_mch1_gdpw-matnr
          iv_werks              = '4900'
*         ls_stpo               =
          iv_get_marterial_type = 'X'
**             iv_active_bom         =
*         iv_bom_alt            = ls_stpo_bom-alt
          iv_active_bom_01      = 'X'
*          iv_usage_probability  = 'X'
        CHANGING
          it_stpo               = lt_stpo_bom_fab.
      READ TABLE  lt_stpo_bom_fab  INTO  DATA(ls_stpo_bom_fab) WITH KEY  mtart  = 'ZFAB'  .
      IF sy-subrc EQ 0.
        " get fab materil charactertics
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input        = ls_stpo_bom_fab-matnr
          IMPORTING
            output       = ls_stpo_bom_fab-matnr
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

        CALL METHOD zcl_zebr_form_dpc_ext=>get_mat_characteristics
          EXPORTING
            iv_matnr           = ls_stpo_bom_fab-matnr
          IMPORTING
            et_allocvaluesnum  = lt_allocvaluesnum
            et_allocvalueschar = lt_allocvalueschar
            et_allocvaluescurr = lt_allocvaluescurr
            et_return          = lt_return.
        READ TABLE lt_allocvaluesnum INTO DATA(ls_num) WITH KEY  charact  = 'GDPW' .
        IF sy-subrc EQ 0.
          er_entity-lot_start_qty  = ls_num-value_from    .

          CLEAR ls_num .
        ENDIF.
      ENDIF.

    ELSE .
      " zfab  lot  material
      READ TABLE  lt_mch1 INTO ls_mch1_gdpw   WITH KEY licha  = er_entity-lot_no
                                                          mtart  = 'ZFAB'.
      IF sy-subrc  EQ  0.
        CALL METHOD zcl_zebr_form_dpc_ext=>get_mat_characteristics
          EXPORTING
            iv_matnr           = ls_mch1_gdpw-matnr
          IMPORTING
            et_allocvaluesnum  = lt_allocvaluesnum
            et_allocvalueschar = lt_allocvalueschar
            et_allocvaluescurr = lt_allocvaluescurr
            et_return          = lt_return.
        READ TABLE lt_allocvaluesnum INTO ls_num WITH KEY  charact  = 'GDPW' .
        IF sy-subrc EQ 0.
          er_entity-lot_start_qty  = ls_num-value_from    .

        ENDIF.

      ENDIF.

    ENDIF .

  ENDMETHOD.


  method EBRDISCARDREASON_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRDISCARDREASON_GET_ENTITYSET
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


    SELECT * FROM zebr_config  INTO TABLE @DATA(lt_zebr_config)  WHERE name  = 'DISCARD_REASON'              "#EC CI_ALL_FIELDS_NEEDED
                                                                  AND type = 'DROP_DOWN'  .
    IF sy-subrc EQ   0  .

      LOOP AT lt_zebr_config INTO DATA(ls_zebr_config)  .
        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entityset>)  .
        <fs_entityset>-discard_reason       = ls_zebr_config-field1  .
        <fs_entityset>-discard_text         = ls_zebr_config-field2 .
      ENDLOOP.

    ENDIF.
  endmethod.


  METHOD EBRDISCARDSET_CREATE_ENTITY.
**TRY.
*CALL METHOD SUPER->EBRDISCARDSET_CREATE_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
**    io_data_provider        =
**  IMPORTING
**    er_entity               =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    DATA   lt_stat_log   TYPE STANDARD TABLE OF  zebr_t_stat_log .

    " get data from screen
    io_data_provider->read_entry_data(
   IMPORTING
   es_data = er_entity ).
    IF er_entity IS NOT INITIAL .
      SELECT * FROM zebr_t_header  INTO TABLE @DATA(lt_ebrheader)  WHERE req_no  =  @er_entity-req_no . "#EC CI_ALL_FIELDS_NEEDED
      IF sy-subrc EQ 0 .
        LOOP AT lt_ebrheader ASSIGNING FIELD-SYMBOL(<fs_ebrheader>) .
          <fs_ebrheader>-req_status_no  = er_entity-req_status_no .
          SELECT SINGLE  field2  FROM zebr_config   INTO @DATA(lv_discard) WHERE name    = 'REQ_STATUS'
                                              AND type    = 'DROP_DOWN'
                                              AND field1  =  @er_entity-req_status_no  .
          IF sy-subrc EQ 0 .
            er_entity-req_status            = lv_discard .
            <fs_ebrheader>-req_status       = lv_discard .
            <fs_ebrheader>-discard_reason   = er_entity-discard_reason.
            CLEAR  lv_discard  .
          ENDIF.

          APPEND INITIAL LINE TO  lt_stat_log ASSIGNING FIELD-SYMBOL(<fs_stat>)  .
          <fs_stat>-req_no         = er_entity-req_no .
          <fs_stat>-req_status_no  = er_entity-req_status_no.
          <fs_stat>-req_status     = er_entity-req_status .
          <fs_stat>-created_by     = sy-uname .
          <fs_stat>-created_at     = sy-uzeit .
          <fs_stat>-created_on     = sy-datum .

        ENDLOOP .
        MODIFY zebr_t_header FROM TABLE lt_ebrheader .
        IF sy-subrc EQ 0.
          COMMIT WORK .
          er_entity-flag  =  'S'  .
          IF lt_stat_log IS NOT INITIAL .
            MODIFY zebr_t_stat_log  FROM TABLE  lt_stat_log .
          ENDIF .

          " send an email when user clicks on discard button
          TRY.
              CALL METHOD zcl_ebr_utility=>email_functionality  " JGAND37207
                EXPORTING
                  im_ebr        = er_entity-req_no
                  im_req_type   = 'N'
                  im_req_status = er_entity-req_status_no
                  im_group      = zcl_ebr_utility=>zemail_grp_discard
*            IMPORTING
*                 et_notif      =
                .
            CATCH cx_abap_invalid_value .
          ENDTRY.

        ELSE.
          er_entity-flag  = 'E'  .
        ENDIF.
      ELSE.
        er_entity-flag  = 'E'  .
      ENDIF .
    ENDIF.
  ENDMETHOD.


  method EBRDISCARDSET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->EBRDISCARDSET_GET_ENTITY
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

  endmethod.


  METHOD EBRDSHIPFACSET_GET_ENTITYSET.

**TRY.
*CALL METHOD SUPER->EBRDSHIPFACSET_GET_ENTITYSET
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
    SELECT  expnr FROM  zcrossref_sto  INTO TABLE @DATA(lt_dship)  WHERE sursys  = '5'
                                                                    AND   flag = '2'  .
    IF sy-subrc EQ 0.
        APPEND  INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_dship>)  .
        <fs_dship>-dc_location   =  ' ' .
      LOOP AT lt_dship INTO DATA(ls_dship)  .
        APPEND  INITIAL LINE TO et_entityset ASSIGNING <fs_dship>  .
        <fs_dship>-dc_location   = ls_dship-expnr  .
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  method EBRFASTTRACKSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRFASTTRACKSET_GET_ENTITYSET
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
       SELECT * from ZEBR_CONFIG  into table @data(lt_zebr_config)  where name  = 'FASTTRACK'                     "#EC CI_ALL_FIELDS_NEEDED
                                                                  and type = 'DROP_DOWN'  .

   loop at lt_zebr_config into data(ls_zebr_config)  .
     APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entityset>)  .
     <fs_entityset>-fasttrack_no  = ls_zebr_config-field2  .
     <fs_entityset>-fasttrack_desc  = ls_zebr_config-field1 .
   ENDLOOP.
  endmethod.


  method EBRFILESET_GET_ENTITY.
if sy-uname eq 'JGAND37207'.

  ENDIF.
  endmethod.


  METHOD EBRFILESET_GET_ENTITYSET.
    DATA:
      it_final   TYPE STANDARD TABLE OF zebr_t_file,
      lt_filters TYPE                   /iwbep/t_mgw_select_option,
      ls_filter  TYPE                   /iwbep/s_mgw_select_option,
      ls_so      TYPE                   /iwbep/s_cod_select_option,
      p_name     TYPE c LENGTH 15,
      lt_req     TYPE TABLE OF          selopt,
      ls_req     TYPE                   selopt.

    IF it_filter_select_options IS NOT INITIAL.
      LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options).
        IF ls_filter_select_options-property EQ 'ReqNo'.
          LOOP AT ls_filter_select_options-select_options INTO DATA(ls_sel) WHERE low IS NOT INITIAL.
            MOVE-CORRESPONDING ls_sel TO ls_req.

            INSERT ls_req INTO TABLE lt_req.
            CLEAR ls_req.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.


*    READ TABLE it_key_tab INTO DATA(ls_key_tab) INDEX 2.
*    IF sy-subrc EQ 0.
*      DATA(lv_req) = ls_key_tab-value.
*    ENDIF.
    SELECT
      req_no
      filename
      sydate
      sytime
      value
      mimetype
    FROM zebr_t_file
    INTO TABLE et_entityset
      WHERE req_no IN lt_req.
  ENDMETHOD.


  method EBRFINISHFACSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRFINISHFACSET_GET_ENTITYSET
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
        data : lt_fac  type zcl_zebr_form_mpc_ext=>tt_ebrfinishfac   .
            APPEND INITIAL LINE TO lt_fac ASSIGNING FIELD-SYMBOL(<fs_fac>)  .
    <fs_fac>-mfrpn  = ' '   .

    et_entityset  = lt_fac .

  endmethod.


  METHOD EBRGETFACILITYSE_GET_ENTITYSET.

    TYPES: ts_stpo_level  TYPE  zbom_com_level,
           tty_stpo_level TYPE  ztt_bom_com_level.
    DATA : lv_mfrpn      TYPE mara-mfrpn,
           lt_stpo       TYPE  tty_stpo_level,
           lv_matnr_op   TYPE matnr,
           lrt_mfrpn     TYPE RANGE OF mfrpn,
           lt_fac        TYPE zcl_zebr_form_mpc_ext=>tt_ebrgetfacility,
           lv_asy_index  TYPE i,
           lv_bump_index TYPE i,
           lv_pbs_index  TYPE i,
           lv_fert_index TYPE i,
           lv_test_index TYPE i,
           lv_srt_index  TYPE i,
           lv_index      TYPE i,
           lv_dc_index   TYPE i,
           lv_mpw        TYPE zcl_zebr_form_mpc_ext=>ts_ebrgetfacility-mpw_wafer,
           lt_stpo_route TYPE  ztt_bom_com_level,
           lv_pbs_matnr  TYPE matnr,
           ls_fac        TYPE zcl_zebr_form_mpc_ext=>ts_ebrgetfacility.

    LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options)  .
      IF ls_filter_select_options-property  = 'Mfrpn'  .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_mfrpn)  INDEX 1.
        IF sy-subrc EQ  0 .
          TRANSLATE ls_filter_mfrpn-low TO UPPER CASE .
          TRANSLATE ls_filter_mfrpn-high TO UPPER CASE .

          CALL METHOD cl_http_utility=>unescape_url
            EXPORTING
              escaped   = ls_filter_mfrpn-low
            RECEIVING
              unescaped = ls_filter_mfrpn-low.
          APPEND INITIAL LINE TO lrt_mfrpn ASSIGNING FIELD-SYMBOL(<fr_mfrpn>).
          <fr_mfrpn>-sign   = ls_filter_mfrpn-sign .
          <fr_mfrpn>-option = ls_filter_mfrpn-option  .
          <fr_mfrpn>-low    = ls_filter_mfrpn-low  .
          <fr_mfrpn>-high   = ls_filter_mfrpn-high  .

        ENDIF.
      ENDIF.

      IF ls_filter_select_options-property  = 'MpwWafer'  .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_mpw)  INDEX 1.
        IF sy-subrc EQ  0 .
          TRANSLATE ls_filter_mpw-low TO UPPER CASE .
          TRANSLATE ls_filter_mpw-high TO UPPER CASE .

          lv_mpw  = ls_filter_mpw-low .

        ENDIF.
      ENDIF.
    ENDLOOP.


    SELECT SINGLE  matnr, mfrpn FROM  mara  INTO @DATA(ls_mara)  WHERE mfrpn  IN @lrt_mfrpn.
    " get bom number from mast table basen on  material number form screen
    CALL METHOD zcl_bom_explode_util=>bom_explode
      EXPORTING
        iv_matnr              = ls_mara-matnr
        iv_werks              = '4900'
*       ls_stpo               =
        iv_get_marterial_type = 'X'
        iv_active_bom_01      = 'X'
*       iv_usage_probability  = 'X'
      CHANGING
        it_stpo               = lt_stpo.

    "  delete  lower level sort materials  in BOM  when multi level sort is available in BOM
    " identify  multi level sort with  material group   with highesh    sort material group  133 or
    "  higher material   and  lower  level material is sort

    IF lt_stpo IS  NOT INITIAL .
      LOOP AT lt_stpo  INTO  DATA(ls_stpo_mult_srt)  .
        DATA(lv_tabix)  = sy-tabix .
        IF ls_stpo_mult_srt-higher_mtart  =  'ZSRT'    AND ls_stpo_mult_srt-mtart  = 'ZSRT'  .
          DELETE   lt_stpo  index  lv_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.


    " get die coat vendors
    IF lt_stpo IS NOT INITIAL  .
      SELECT  *  FROM t415t   INTO TABLE @DATA(lt_t415t)  FOR ALL ENTRIES IN  @lt_stpo  WHERE stlst  = @lt_stpo-bom_status
                                                                                        AND  spras  = @sy-langu .

      SELECT a~matnr , a~matkl , b~werks , b~lifnr   INTO TABLE @DATA(lt_diecoat)  FROM mara AS a INNER JOIN eord AS b
                                                          ON a~matnr   = b~matnr
                                                       FOR ALL ENTRIES IN @lt_stpo
                                                      WHERE a~matnr = @lt_stpo-matnr
                                                       AND  a~matkl  =  '119'  .
      IF sy-subrc EQ 0.
        LOOP AT lt_diecoat  INTO DATA(ls_diecoat)   .
          READ TABLE   lt_stpo INTO DATA(ls_stpo_dc)  WITH KEY matnr  = ls_diecoat-matnr .
          IF sy-subrc EQ 0.
            ls_stpo_dc-mtart  = 'ZDC'  .      " temporarty mtart  which is used at end to bifucate facilities
            ls_stpo_dc-lifnr  = ls_diecoat-lifnr .
            APPEND ls_stpo_dc   TO lt_stpo  .
          ENDIF .
        ENDLOOP .
      ENDIF .

      " get route vendors
      SELECT a~matnr ,
             a~werks ,
             a~plnty ,
             a~plnnr ,
             a~plnal ,
             b~zaehl ,
             b~datuv ,
             b~statu ,
             b~ktext INTO TABLE @DATA(lt_mapl)  FROM mapl   AS a  INNER JOIN plko  AS b ON    a~plnty  =   b~plnty
                                                                                       AND   a~plnnr  =   b~plnnr
                                                                                       AND   a~plnal  =   b~plnal
                                                                                  FOR ALL ENTRIES IN @lt_stpo
                                                                                   WHERE a~matnr  = @lt_stpo-matnr
                                                                                    AND  a~plnty  = 'N'
                                                                                    AND  b~verwe   NE 'CST'
                                                                                    AND  a~loekz   EQ  ' '  .
*                                                                                    and  b~statu  ne  '5'.
      IF sy-subrc EQ 0.
        SORT lt_mapl  BY plnnr  plnal ASCENDING  datuv  DESCENDING .
        DELETE ADJACENT DUPLICATES FROM  lt_mapl  COMPARING plnnr plnal  .
        DELETE lt_mapl WHERE statu EQ  '5' .
        SELECT werks , name1  FROM t001w INTO TABLE @DATA(lt_t001w) FOR ALL ENTRIES IN @lt_mapl WHERE
                          werks  =  @lt_mapl-werks  .
        IF sy-subrc EQ 0.
          SORT lt_t001w   BY werks  .
        ENDIF.
        SELECT a~plnty  ,
               a~plnnr  ,
               a~plnal  ,
               a~plnkn  ,
               a~zaehl  ,
               b~datuv  ,
               b~loekz  ,
               b~vornr  ,
               b~lifnr  ,
               b~infnr  INTO TABLE @DATA(lt_plas_plpo)  FROM plas AS a  INNER JOIN plpo AS b
                                                ON    a~plnty   =   b~plnty
                                               AND    a~plnnr   =   b~plnnr
                                               AND    a~plnkn   =   b~plnkn
                                     FOR ALL ENTRIES IN  @lt_mapl
                                             WHERE  a~plnty  =  @lt_mapl-plnty
                                              AND   a~plnnr  =  @lt_mapl-plnnr
                                              AND   a~plnal  =  @lt_mapl-plnal
                                              AND   b~werks  = @lt_mapl-werks.
        IF sy-subrc EQ 0.
          SORT lt_plas_plpo  BY plnty plnnr plnal ASCENDING datuv  DESCENDING  .
          DELETE ADJACENT DUPLICATES FROM lt_plas_plpo   COMPARING plnty plnnr plnal .
          DELETE lt_plas_plpo WHERE loekz  =  'X'  .


          SELECT infnr , matnr  ,lifnr FROM  eina INTO TABLE  @DATA(lt_eina)   FOR ALL ENTRIES IN  @lt_plas_plpo  WHERE infnr  = @lt_plas_plpo-infnr .
          IF sy-subrc EQ 0.
            SORT lt_eina  BY infnr .
          ENDIF.
        ENDIF.
        " add DC facilities from
        SELECT * FROM zebr_config  INTO TABLE @DATA(lt_zebr_config)  WHERE name  = 'DESIGN_CENTER' "#EC CI_ALL_FIELDS_NEEDED
                                                                      AND type = 'DROP_DOWN'  .

        LOOP AT lt_zebr_config INTO DATA(ls_zebr_config)  .
          READ TABLE  lt_stpo TRANSPORTING NO FIELDS WITH KEY  mtart = 'ZPBS'  .
          IF sy-subrc EQ 0.
            APPEND INITIAL LINE TO lt_stpo ASSIGNING FIELD-SYMBOL(<fs_stpo>)  .
            <fs_stpo>-mtart        = 'ZPBS' .
            <fs_stpo>-lifnr        = ls_zebr_config-field1 .
            <fs_stpo>-lifnr_desc   = ls_zebr_config-field2 .
          ENDIF.

          READ TABLE  lt_stpo TRANSPORTING NO FIELDS WITH KEY  mtart = 'ZTST'  .
          IF sy-subrc EQ 0.
            APPEND INITIAL LINE TO lt_stpo ASSIGNING <fs_stpo> .
            <fs_stpo>-mtart        = 'ZTST' .
            <fs_stpo>-lifnr        = ls_zebr_config-field1 .
            <fs_stpo>-lifnr_desc   = ls_zebr_config-field2 .
          ENDIF.

          READ TABLE  lt_stpo TRANSPORTING NO FIELDS WITH KEY  mtart = 'FERT'  .
          IF sy-subrc EQ 0.
            APPEND INITIAL LINE TO lt_stpo ASSIGNING <fs_stpo>  .
            <fs_stpo>-mtart        = 'FERT' .
            <fs_stpo>-lifnr        = ls_zebr_config-field1 .
            <fs_stpo>-lifnr_desc   = ls_zebr_config-field2 .
          ENDIF.

        ENDLOOP.

        " build route vendors
        LOOP AT lt_stpo INTO  DATA(ls_stpo_route)  .
          LOOP AT lt_mapl  INTO DATA(ls_mapl)  WHERE matnr  = ls_stpo_route-matnr.
            IF ls_stpo_route-mtart  = 'ZASY'  OR ls_stpo_route-mtart  = 'ZBMP' .  "  route vendor's are required for assembly and bump
*              " get the rout of last operation .
*              DATA(lt_plas_plpo_tmp)  =  lt_plas_plpo .
*              DELETE lt_plas_plpo_tmp WHERE plnnr NE ls_mapl-plnnr  .
*              DELETE lt_plas_plpo_tmp WHERE plnal NE ls_mapl-plnal .
*              SORT  lt_plas_plpo_tmp  BY vornr  DESCENDING .
*              READ TABLE lt_plas_plpo_tmp  INTO DATA(ls_plas_plpo_tmp)  INDEX 1 .
*              IF sy-subrc EQ 0.
*                ls_stpo_route-lifnr  =  ls_plas_plpo_tmp-lifnr .
*                          .
*                APPEND ls_stpo_route  TO lt_stpo_route .
*              ENDIF .
*              CLEAR : lt_plas_plpo_tmp[] ,
*                      ls_plas_plpo_tmp  .
            ELSE.
              " get vendor in any opertation where vendor is avialble
              " if vendor is not availaable put  plant as vendor  as it represnts internal
              IF lv_mpw IS NOT INITIAL .
                DATA(lt_plas_plpo_tmp)  =  lt_plas_plpo .
                DELETE lt_plas_plpo_tmp WHERE plnnr NE ls_mapl-plnnr  .
                DELETE lt_plas_plpo_tmp WHERE plnal NE ls_mapl-plnal .
                DELETE lt_plas_plpo_tmp WHERE lifnr  EQ space .
                READ TABLE lt_plas_plpo_tmp  INTO DATA(ls_plas_plpo_tmp)    INDEX 1 .
                IF sy-subrc EQ 0  AND ls_plas_plpo_tmp-lifnr  IS NOT INITIAL .

                  ls_stpo_route-lifnr  =   ls_plas_plpo_tmp-lifnr  .
                  .
                  APPEND ls_stpo_route  TO lt_stpo_route .
                ELSEIF  sy-subrc EQ 0  AND ls_plas_plpo_tmp-infnr IS NOT INITIAL .
                  READ TABLE lt_eina INTO DATA(ls_eina)  WITH KEY infnr  = ls_plas_plpo_tmp-infnr  BINARY SEARCH .
                  IF sy-subrc EQ 0.
                    ls_stpo_route-lifnr   = ls_eina-lifnr .
                  ENDIF.

                ELSE.
                  ls_stpo_route-lifnr   = ls_mapl-werks .
                  READ TABLE  lt_t001w  INTO DATA(ls_t001w)  WITH KEY werks  = ls_mapl-werks .
                  IF sy-subrc EQ 0 .
                    IF ls_stpo_route-mtart  EQ 'ZPBS' .
                      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
                        EXPORTING
                          input  = ls_mapl-matnr
                        IMPORTING
                          output = lv_pbs_matnr.
                      " show description  for pBS material  _'p1'
                      DATA(lv_len)  = strlen( lv_pbs_matnr )  .
                      lv_len  = lv_len - 2 .
                      " to show description in below format  P1 and P2  (P1)
                      IF lv_pbs_matnr IS NOT INITIAL .
                        ls_stpo_route-lifnr_desc   = '(' && lv_pbs_matnr+lv_len(2) && ')' && ls_t001w-name1  .
                      ENDIF.
                    ELSE.
                      ls_stpo_route-lifnr_desc   =  ls_t001w-name1  .
                    ENDIF.
                  ENDIF.
                  APPEND ls_stpo_route  TO lt_stpo_route .
                ENDIF.
                CLEAR  :   lt_plas_plpo_tmp[] ,
                           ls_plas_plpo_tmp  .
              ELSE.
                " show route for other than  mpw
                CONCATENATE ls_mapl-plnnr  ls_mapl-plnal  INTO  ls_stpo_route-lifnr .
                IF ls_stpo_route-mtart  EQ 'ZPBS' .
                  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
                    EXPORTING
                      input  = ls_mapl-matnr
                    IMPORTING
                      output = lv_pbs_matnr.
                  " show description  for pBS material  _'p1'
                  lv_len  = strlen( lv_pbs_matnr )  .
                  lv_len  = lv_len - 2 .
                  " to show description in below format  P1 and P2  (P1)
                  IF lv_pbs_matnr IS NOT INITIAL .
                    ls_stpo_route-lifnr_desc   = '(' && lv_pbs_matnr+lv_len(2) && ')' && ls_mapl-ktext  .
                  ENDIF.
                ELSE.
                  ls_stpo_route-lifnr_desc   =  ls_mapl-ktext  .
                ENDIF.
                APPEND ls_stpo_route  TO lt_stpo_route .
              ENDIF.
            ENDIF.
            CLEAR ls_mapl .
          ENDLOOP .
          CLEAR   ls_stpo_route .
        ENDLOOP.
        APPEND LINES OF lt_stpo_route  TO lt_stpo .


      ENDIF.
      " get vendor description
      SELECT lifnr , name3  FROM lfa1 INTO TABLE @DATA(lt_lfa1)
        FOR ALL ENTRIES IN @lt_stpo WHERE lifnr   =  @lt_stpo-lifnr  .

      IF sy-subrc EQ 0 .
        LOOP AT lt_stpo ASSIGNING FIELD-SYMBOL(<fs_stpo_lifnr>)  .
          READ TABLE lt_lfa1  INTO DATA(ls_lfa1)  WITH KEY lifnr = <fs_stpo_lifnr>-lifnr  .
          IF sy-subrc EQ  0 .
            READ TABLE  lt_t415t  INTO DATA(ls_t415t)   WITH KEY  stlst  =   <fs_stpo_lifnr>-bom_status  .

            <fs_stpo_lifnr>-lifnr_desc   =   ls_lfa1-name3 && '(' && ls_t415t-sttxt && ')' .
          ENDIF.
        ENDLOOP .
      ENDIF.
    ENDIF.
    "  add new as vendor under aasembly and bump
    READ TABLE lt_stpo INTO DATA(ls_stpo_new_vendor)  WITH KEY  mtart  = 'ZASY'  .
    IF sy-subrc EQ 0.
      ls_stpo_new_vendor-lifnr      =  '  '  .
      ls_stpo_new_vendor-lifnr_desc =  '  ' .
      APPEND   ls_stpo_new_vendor  TO  lt_stpo  .

      APPEND   ls_stpo_new_vendor  TO  lt_stpo  .
      ls_stpo_new_vendor-lifnr      = 'NEW'  .
      ls_stpo_new_vendor-lifnr_desc =  'NEW'.
      APPEND   ls_stpo_new_vendor  TO  lt_stpo  .

      ls_stpo_new_vendor-lifnr      = 'ANY'  .
      ls_stpo_new_vendor-lifnr_desc =  'ANY'.
      INSERT ls_stpo_new_vendor  INTO lt_stpo  INDEX 1 .
      CLEAR  ls_stpo_new_vendor  .
    ENDIF.


    READ TABLE lt_stpo INTO ls_stpo_new_vendor  WITH KEY  mtart  = 'ZBMP'  .
    IF sy-subrc EQ 0.
      ls_stpo_new_vendor-lifnr      =  '  '  .
      ls_stpo_new_vendor-lifnr_desc =  '   ' .
      APPEND   ls_stpo_new_vendor  TO  lt_stpo  .

      ls_stpo_new_vendor-lifnr      = 'NEW'  .
      ls_stpo_new_vendor-lifnr_desc =  'NEW'.
      APPEND   ls_stpo_new_vendor  TO  lt_stpo   .

      ls_stpo_new_vendor-lifnr          = 'ANY'  .
      ls_stpo_new_vendor-lifnr_desc      =  'ANY'.
      INSERT ls_stpo_new_vendor  INTO lt_stpo  INDEX 1 .
      CLEAR  ls_stpo_new_vendor  .
    ENDIF.


    " below logic is written  to modify from  itab1  fromat to itab 2 format .
*        itab1                                   itab2
*     lifnr |lifnr desc                  assylifnr |  assy lifnr desc |  bumplifnr | bumpdesc| ZPBS | ZPBSDESC|
*    DELETE lt_stpo WHERE lifnr EQ space .
    LOOP AT lt_stpo INTO  DATA(ls_stpo)  WHERE lifnr NE space  .
      ls_fac-mfrpn  =  ls_mara-mfrpn.
      CASE ls_stpo-mtart  .
        WHEN 'ZSRT'  .
          READ TABLE  lt_fac  TRANSPORTING NO FIELDS  WITH KEY sort_lifnr  = ls_stpo-lifnr  .
          IF sy-subrc  NE 0  .
            DATA(lv_matnr_opmat)   =   ls_stpo-matnr  .
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
              EXPORTING
                input  = lv_matnr_opmat "ls_stpo-matnr
              IMPORTING
                output = lv_matnr_opmat.

            lv_srt_index       =  lv_srt_index  + 1 .
            ls_fac-sort_lifnr  =  ls_stpo-lifnr .

            IF lv_mpw IS INITIAL .
              ls_fac-sort_name1 =  ls_stpo-lifnr_desc  &&  '('  && lv_matnr_opmat && ')' .
            ELSE.
              ls_fac-sort_name1  =  ls_stpo-lifnr_desc  .
            ENDIF.
            ls_fac-bnd_number  =   ls_stpo-doknr .
            IF lv_srt_index LE lv_index  .
              MODIFY lt_fac FROM ls_fac INDEX  lv_srt_index  TRANSPORTING sort_lifnr sort_name1  .
            ELSE.
              APPEND ls_fac TO lt_fac .
            ENDIF.
            IF lv_srt_index  > lv_index  OR lv_index   IS INITIAL .
              lv_index  = lv_srt_index .
            ENDIF.
            CLEAR  : lv_matnr_opmat .
          ENDIF.
        WHEN 'ZASY'  .
          READ TABLE  lt_fac  TRANSPORTING NO FIELDS  WITH KEY assy_lifnr  = ls_stpo-lifnr  .
          IF sy-subrc  NE 0  .
            lv_asy_index       =  lv_asy_index  + 1 .
            ls_fac-assy_lifnr  =  ls_stpo-lifnr .
            ls_fac-assy_name1  =  ls_stpo-lifnr_desc  .
            ls_fac-bnd_number  =   ls_stpo-doknr .
            IF lv_asy_index LE lv_index  .
              MODIFY lt_fac FROM ls_fac INDEX  lv_asy_index  TRANSPORTING assy_lifnr assy_name1  .
            ELSE.
              APPEND ls_fac TO lt_fac .
            ENDIF.
            IF lv_asy_index  > lv_index  OR lv_index   IS INITIAL .
              lv_index  = lv_asy_index .
            ENDIF.
          ENDIF.

        WHEN  'ZBMP'  .
          READ TABLE  lt_fac  TRANSPORTING NO FIELDS  WITH KEY bump_lifnr  = ls_stpo-lifnr  .
          IF sy-subrc NE 0  .
            lv_bump_index  =  lv_bump_index  + 1 .

            ls_fac-bump_lifnr  =  ls_stpo-lifnr .
            ls_fac-bump_name1  =  ls_stpo-lifnr_desc .
            IF lv_bump_index LE lv_index  .
              MODIFY lt_fac FROM ls_fac INDEX  lv_bump_index  TRANSPORTING bump_lifnr bump_name1  .
            ELSE.
              APPEND ls_fac TO lt_fac .
            ENDIF.
            IF lv_bump_index  > lv_index  OR lv_index   IS INITIAL .
              lv_index  = lv_bump_index .
            ENDIF.
          ENDIF.

        WHEN 'ZPBS'  .
          READ TABLE  lt_fac  TRANSPORTING NO FIELDS  WITH KEY pbs_lifnr  = ls_stpo-lifnr  .
          IF sy-subrc  NE 0  .
            lv_pbs_index  =  lv_pbs_index  + 1 .

            ls_fac-pbs_lifnr  =  ls_stpo-lifnr .
            ls_fac-pbs_name1  =  ls_stpo-lifnr_desc  .
            IF lv_pbs_index LE lv_index  .
              MODIFY lt_fac FROM ls_fac INDEX  lv_pbs_index  TRANSPORTING pbs_lifnr pbs_name1  .
            ELSE.
              APPEND ls_fac TO lt_fac .
            ENDIF.

            IF lv_pbs_index  > lv_index  OR lv_index   IS INITIAL .
              lv_index  = lv_pbs_index .
            ENDIF.
          ENDIF.

        WHEN 'FERT'  .
          READ TABLE  lt_fac  TRANSPORTING NO FIELDS  WITH KEY fert_lifnr  = ls_stpo-lifnr  .
          IF sy-subrc  NE 0  .
            lv_fert_index  =  lv_fert_index  + 1 .

            ls_fac-fert_lifnr  =  ls_stpo-lifnr .
            ls_fac-fert_name1  =  ls_stpo-lifnr_desc  .
            IF lv_fert_index LE lv_index  .
              MODIFY lt_fac FROM ls_fac INDEX  lv_fert_index  TRANSPORTING fert_lifnr fert_name1  .
            ELSE.
              APPEND ls_fac TO lt_fac .
            ENDIF.

            IF lv_fert_index  > lv_index  OR lv_index   IS INITIAL .
              lv_index  = lv_fert_index .
            ENDIF.
          ENDIF.

        WHEN 'ZTST' .
          READ TABLE  lt_fac  TRANSPORTING NO FIELDS  WITH KEY test_lifnr  = ls_stpo-lifnr  .
          IF sy-subrc  NE 0  .
            lv_test_index  =  lv_test_index  + 1 .

            ls_fac-test_lifnr  =  ls_stpo-lifnr .
*            ls_fac-test_name1  =  ls_stpo-lifnr_desc .
            DATA(lv_matnr_optst)   =   ls_stpo-matnr  .
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
              EXPORTING
                input  = lv_matnr_optst "ls_stpo-matnr
              IMPORTING
                output = lv_matnr_optst.
            IF lv_mpw IS INITIAL .
              ls_fac-test_name1 =  ls_stpo-lifnr_desc  &&  '('  && lv_matnr_optst && ')' .
            ELSE.
              ls_fac-test_name1  =  ls_stpo-lifnr_desc  .
            ENDIF.
            IF lv_test_index LE lv_index  .
              MODIFY lt_fac FROM ls_fac INDEX  lv_test_index  TRANSPORTING test_lifnr test_name1  .
            ELSE.
              APPEND ls_fac TO lt_fac .
            ENDIF.

            IF lv_test_index  > lv_index  OR lv_index   IS INITIAL .
              lv_index  = lv_test_index .
            ENDIF.
          ENDIF.
        WHEN 'ZDC'  .
          READ TABLE  lt_fac  TRANSPORTING NO FIELDS  WITH KEY db_lifnr  = ls_stpo-lifnr  .
          IF sy-subrc  NE 0  .
            lv_dc_index   = lv_dc_index  + 1  .
            ls_fac-db_lifnr   = ls_stpo-lifnr .
            ls_fac-db_name1   = ls_stpo-lifnr_desc.
            IF lv_dc_index  LE lv_index  .
              MODIFY lt_fac FROM ls_fac INDEX  lv_dc_index  TRANSPORTING db_lifnr db_name1  .
            ELSE .
              APPEND ls_fac TO lt_fac .
            ENDIF.
            IF lv_dc_index  > lv_index  OR lv_index   IS INITIAL .
              lv_index  = lv_dc_index .
            ENDIF.
          ENDIF.

        WHEN OTHERS.
      ENDCASE .

      CLEAR : ls_stpo ,
              ls_fac,
              lv_matnr_optst,
              lv_matnr_opmat.
    ENDLOOP.
    " below '*'  is just for front end designing and mapping  to avoid e
    READ TABLE   lt_fac  ASSIGNING FIELD-SYMBOL(<fs_fac>)  INDEX 1 .  .
    IF sy-subrc  EQ   0 .
      CLEAR  : ls_fac .
      IF <fs_fac>-assy_lifnr  IS INITIAL  .
        READ TABLE lt_stpo  INTO  ls_stpo WITH KEY mtart  =  'ZASY'  .
        IF sy-subrc EQ 0 .
          ls_fac-assy_lifnr  =  ' ' .
        ELSE.
          ls_fac-assy_lifnr  = '*'  .
        ENDIF.
      ELSE.
        ls_fac-assy_lifnr  =  ' ' .
      ENDIF.

      IF <fs_fac>-bump_lifnr  IS INITIAL  .
        READ TABLE lt_stpo  INTO  ls_stpo WITH KEY mtart  =  'ZBMP'  .
        IF sy-subrc EQ 0 .
          ls_fac-bump_lifnr  =  ' ' .
        ELSE.
          ls_fac-bump_lifnr  = '*'  .
        ENDIF.
      ELSE.
        ls_fac-bump_lifnr  =  ' ' .
      ENDIF.


      IF <fs_fac>-pbs_lifnr  IS INITIAL  .
        READ TABLE lt_stpo  INTO  ls_stpo WITH KEY mtart  =  'ZPBS'  .
        IF sy-subrc EQ 0 .
          ls_fac-pbs_lifnr    =  ' ' .
        ELSE.
          ls_fac-pbs_lifnr  = '*' .
        ENDIF.
      ELSE.
        ls_fac-pbs_lifnr    =  ' ' .
      ENDIF.

      IF <fs_fac>-test_lifnr  IS INITIAL  .
        READ TABLE lt_stpo  INTO  ls_stpo WITH KEY mtart  =  'ZTST'  .
        IF sy-subrc EQ 0 .
          ls_fac-test_lifnr  =  ' '  .
        ELSE.
          ls_fac-test_lifnr  = '*' .
        ENDIF.
      ELSE.
        ls_fac-test_lifnr  =  ' '  .
      ENDIF.


      IF <fs_fac>-fert_lifnr  IS INITIAL  .
        READ TABLE lt_stpo  INTO  ls_stpo WITH KEY mtart  =  'FERT'  .
        IF sy-subrc EQ 0 .
          ls_fac-fert_lifnr  =  ' '  .
        ELSE.
          ls_fac-fert_lifnr  = '*' .
        ENDIF.
      ELSE.
        ls_fac-fert_lifnr  =  ' '  .
      ENDIF.
      INSERT ls_fac INTO lt_fac   INDEX 1 .
      CLEAR ls_fac .
    ELSE.
      APPEND INITIAL LINE TO lt_fac ASSIGNING <fs_fac>  .
      <fs_fac>-assy_lifnr  = '*'  .
      <fs_fac>-bump_lifnr  = '*'  .
      <fs_fac>-pbs_lifnr   = '*' .
      <fs_fac>-test_lifnr  = '*' .
      <fs_fac>-fert_lifnr  = '*' .
    ENDIF.
    et_entityset  = lt_fac .
    CLEAR : lv_asy_index ,
            lv_bump_index ,
            lv_pbs_index  ,
            lv_fert_index ,
            lv_test_index ,
            lv_mpw.

  ENDMETHOD.


  method EBRHEADERSET_CREATE_ENTITY.
**TRY.
*CALL METHOD SUPER->EBRHEADERSET_CREATE_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
**    io_data_provider        =
**  IMPORTING
**    er_entity               =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  endmethod.


  method EBRHEADERSET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->EBRHEADERSET_GET_ENTITY
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
   READ TABLE It_key_tab   into data(ls_key_tab) with KEY name = 'ReqNo'  .
    if sy-subrc eq 0.
    er_entity-req_no   = ls_key_tab-value  .
    ENDIF.
  endmethod.


  method EBRHEADERSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRHEADERSET_GET_ENTITYSET
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
    loop AT It_key_tab INTO data(ls_key_tab) WHERE NAME  = 'ReqNo'  .
    APPEND INITIAL LINE TO ET_ENTITYSET ASSIGNING FIELD-SYMBOL(<fs_entityset>)  .
    <fs_entityset>-req_no   = ls_key_tab-value   .
    clear ls_key_tab.
    ENDLOOP.

  endmethod.


  method EBRLOTINFOSET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->EBRLOTINFOSET_GET_ENTITY
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
    READ TABLE It_key_tab   into data(ls_key_tab) with KEY name = 'ReqNo'  .
    if sy-subrc eq 0.
    er_entity-req_no   = ls_key_tab-value  .
    ENDIF.
  endmethod.


  METHOD EBRLOTINFOSET_GET_ENTITYSET.

**TRY.
*CALL METHOD SUPER->EBRLOTINFOSET_GET_ENTITYSET
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
* io_data_provider->read_entry_data(
*IMPORTING
*es_data = et_entityset ).
    LOOP AT it_key_tab INTO data(ls_key_tab) WHERE name  = 'ReqNo'  .
      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entityset>)  .
      <fs_entityset>-req_no   = ls_key_tab-value   .
      CLEAR ls_key_tab.
    ENDLOOP.

  ENDMETHOD.


  METHOD EBRLOTSEARCHSET_GET_ENTITYSET.

    TYPES:
      ts_stpo_level TYPE zbom_com_level.
    TYPES:
      tty_stpo_level TYPE ztt_bom_com_level.
    DATA : lv_mfrpn           TYPE mara-mfrpn,
           lt_stpo            TYPE tty_stpo_level,
           lt_stpo_phantom    TYPE tty_stpo_level,
           lv_matnr_op        TYPE matnr,
           lt_bom_details     TYPE ztt_bom_item,
           lt_bom_ip_out      TYPE ztt_bom_ip_out,
           lv_objkey          TYPE bapi1003_key-object,
           lrt_licha          TYPE RANGE OF lichn,
           lrt_charg          TYPE RANGE OF charg_d,
           lrt_mfrpn          TYPE RANGE OF  mfrpn,
           lrt_fabmat         TYPE RANGE  OF matnr,
           lrt_die            TYPE RANGE OF  matnr,
           lv_die             TYPE matnr,
           lt_allocvaluesnum  TYPE TABLE OF  bapi1003_alloc_values_num,
           lt_allocvalueschar TYPE  TABLE OF bapi1003_alloc_values_char,
           lt_allocvaluescurr TYPE  TABLE OF bapi1003_alloc_values_curr,
           lt_allocations     TYPE TABLE OF api_kssk,
           lv_classnum        TYPE  bapi1003_key-classnum,
           lv_classtype       TYPE  bapi1003_key-classtype,
           lt_return_t        TYPE TABLE OF bapiret2.

    LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options)  .
      IF ls_filter_select_options-property  = 'LotNo'  .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_lotno)  INDEX 1.
        IF sy-subrc EQ  0 AND  ls_filter_lotno-low  IS NOT INITIAL.

          TRANSLATE ls_filter_lotno-low TO UPPER CASE .
          TRANSLATE ls_filter_lotno-high TO UPPER CASE .
          APPEND INITIAL LINE TO lrt_licha ASSIGNING FIELD-SYMBOL(<fr_licha>).
          <fr_licha>-sign   = ls_filter_lotno-sign .
          <fr_licha>-option = ls_filter_lotno-option  .
          <fr_licha>-low    = ls_filter_lotno-low  .
          <fr_licha>-high   = ls_filter_lotno-high  .
        ENDIF.
      ENDIF.

      IF ls_filter_select_options-property  = 'OutputDieType'  .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_die_type)  INDEX 1.
        IF sy-subrc EQ  0 .
          DATA(lv_die_type)   = ls_filter_die_type-low  .
        ENDIF.
      ENDIF.

      IF ls_filter_select_options-property  = 'OutputDieRev'  .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_die_rev)  INDEX 1.
        IF sy-subrc EQ  0 .
          DATA(lv_die_rev)   = ls_filter_die_rev-low  .
        ENDIF.
      ENDIF.
      IF ls_filter_select_options-property  = 'OutputDieType'    OR  ls_filter_select_options-property  = 'OutputDieRev'  .
        IF lv_die_type   IS NOT INITIAL AND lv_die_rev IS NOT INITIAL .
          lv_die  = lv_die_type && '-'  &&  lv_die_rev  .
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input        = lv_die
            IMPORTING
              output       = lv_die
            EXCEPTIONS
              length_error = 1
              OTHERS       = 2.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.
*
        ENDIF.
      ENDIF.


      IF ls_filter_select_options-property  = 'Mfrpn'  .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_mfrpn)  INDEX 1.
        IF sy-subrc EQ  0 .
          IF ls_filter_mfrpn-low IS NOT INITIAL .
            TRANSLATE ls_filter_mfrpn-low TO UPPER CASE .
            TRANSLATE ls_filter_mfrpn-high TO UPPER CASE .
            CALL METHOD cl_http_utility=>unescape_url
              EXPORTING
                escaped   = ls_filter_mfrpn-low
              RECEIVING
                unescaped = ls_filter_mfrpn-low.
            APPEND INITIAL LINE TO lrt_mfrpn ASSIGNING FIELD-SYMBOL(<fr_mfrpn>).
            <fr_mfrpn>-sign   = ls_filter_mfrpn-sign .
            <fr_mfrpn>-option = ls_filter_mfrpn-option  .
            <fr_mfrpn>-low    = ls_filter_mfrpn-low  .
            <fr_mfrpn>-high   = ls_filter_mfrpn-high  .
          ENDIF.
        ENDIF.
      ENDIF.


      IF ls_filter_select_options-property  = 'Batch'  .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_batch)  INDEX 1.
        IF sy-subrc EQ  0 .
          IF  ls_filter_batch-low   IS NOT INITIAL .
            TRANSLATE ls_filter_batch-low TO UPPER CASE .
            TRANSLATE ls_filter_batch-high TO UPPER CASE .
            APPEND INITIAL LINE TO lrt_charg ASSIGNING FIELD-SYMBOL(<fr_charg>).
            <fr_charg>-sign   = ls_filter_batch-sign .
            <fr_charg>-option = ls_filter_batch-option  .
            <fr_charg>-low    = ls_filter_batch-low  .
            <fr_charg>-high   = ls_filter_batch-high  .
          ENDIF.
        ENDIF.
      ENDIF.


      IF ls_filter_select_options-property  = 'FabLotMat'  .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_fablotmat)  INDEX 1.
        IF sy-subrc EQ  0 .
          IF  ls_filter_fablotmat-low   IS NOT INITIAL .
            TRANSLATE ls_filter_batch-low TO UPPER CASE .
            TRANSLATE ls_filter_batch-high TO UPPER CASE .
            APPEND INITIAL LINE TO lrt_fabmat ASSIGNING FIELD-SYMBOL(<fr_fabmat>).
            <fr_fabmat>-sign   = ls_filter_fablotmat-sign .
            <fr_fabmat>-option = ls_filter_fablotmat-option  .
            <fr_fabmat>-low    = ls_filter_fablotmat-low  .
            <fr_fabmat>-high   = ls_filter_fablotmat-high  .
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lrt_licha IS NOT INITIAL  .
      SELECT a~matnr , a~mtart, a~groes , a~mfrpn , b~charg , b~licha  ,c~werks , c~lgort   , c~clabs  ,c~ceinm
       INTO TABLE  @DATA(lt_mara_mch1) FROM  mara  AS a  INNER JOIN mch1  AS b
                                            ON  a~matnr   = b~matnr
                                            INNER JOIN mchb AS c
                                            ON b~matnr  = c~matnr
                                           AND b~charg  = c~charg
                                            WHERE a~mtart  IN ('ZFAB' ,'ZSRT')
                                             AND  b~licha  IN  @lrt_licha .

    ELSEIF lrt_fabmat  IS NOT INITIAL   OR  lrt_charg IS NOT INITIAL .

      SELECT a~matnr  a~mtart a~groes  a~mfrpn  b~charg  b~licha  c~werks c~lgort    c~clabs  c~ceinm
             INTO TABLE  lt_mara_mch1 FROM  mara  AS a  INNER JOIN mch1  AS b
                                                  ON  a~matnr   = b~matnr
                                                  INNER JOIN mchb AS c
                                                  ON b~matnr  = c~matnr
                                                 AND b~charg  = c~charg
                                                 INNER JOIN t320 AS d
                                                  ON    c~werks  =  d~werks
                                                  AND   c~lgort  =  d~lgort
                                                  WHERE a~mfrpn  IN lrt_fabmat
                                                   AND  a~mtart  IN ('ZFAB' ,'ZSRT')
                                                   AND  b~licha  IN  lrt_licha
                                                   AND  b~charg  IN lrt_charg
                                                   AND ( c~clabs    GT  0 OR c~ceinm GT  0 ).

    ELSE.
      SELECT  SINGLE   matnr  ,mfrpn INTO  @DATA(ls_mara)  FROM mara   WHERE mfrpn  IN @lrt_mfrpn  .
      IF sy-subrc EQ 0  .
        " get bom number from mast table based on  material number form screen
        CALL METHOD zcl_bom_explode_util=>bom_explode
          EXPORTING
            iv_matnr              = ls_mara-matnr
            iv_werks              = '4900'
*           ls_stpo               =
            iv_get_marterial_type = 'X'
            iv_active_bom_01      = 'X'
*            iv_usage_probability  = 'X'
          CHANGING
            it_stpo               = lt_stpo.


        IF lt_stpo IS NOT INITIAL .
          "  find  phantom die in  bom   by passing part dietype and dierev to  zdie_type  zdie_revcode
          LOOP AT  lt_stpo INTO DATA(ls_stpo)  WHERE   zdie_type      = lv_die_type
                                                AND    zdie_revcode    =  lv_die_rev .
            APPEND INITIAL LINE TO  lt_stpo_phantom   ASSIGNING  FIELD-SYMBOL(<fs_stpo_phantom>).
            MOVE-CORRESPONDING  ls_stpo  TO <fs_stpo_phantom>  .

          ENDLOOP  .
          " regular case when  lt_stpo_phantom is empty
          IF lt_stpo_phantom IS INITIAL .
            READ TABLE  lt_stpo  INTO DATA(ls_stpo_fab)  WITH KEY matnr  =  lv_die  .
            IF sy-subrc EQ 0.
              DATA(lv_len) = strlen( ls_stpo_fab-level )   .
              lv_len  = lv_len  - 2 .
              READ TABLE  lt_stpo  INTO  DATA(ls_stpo_srt)  WITH KEY level  = ls_stpo_fab-level+0(lv_len)  .
              IF sy-subrc EQ 0.
                DATA(lv_sort_matnr)   = ls_stpo_srt-matnr .
              ENDIF.
            ENDIF.
            " get sort lots and fab lot for selected die
            SELECT a~matnr  a~mtart a~groes
                   a~mfrpn b~charg  b~licha c~werks  c~lgort c~clabs c~ceinm INTO TABLE  lt_mara_mch1
                                            FROM  mara  AS a  INNER JOIN mch1  AS b
                                            ON  a~matnr   = b~matnr
                                                  INNER JOIN mchb AS c
                                                  ON b~matnr  = c~matnr
                                                 AND b~charg  = c~charg
                                                 INNER JOIN t320 AS d
                                                  ON    c~werks  =  d~werks
                                                  AND   c~lgort  =  d~lgort
                                            WHERE a~matnr  IN (lv_sort_matnr,lv_die)
                                            AND  a~mtart  IN ('ZFAB' ,'ZSRT')
                                           AND ( c~clabs    GT  0 OR c~ceinm GT  0 ) .
            IF sy-subrc EQ 0.

            ENDIF.

          ELSE.
            "  phantom die  is  available
            SELECT a~matnr  a~mtart   a~groes a~mfrpn
                   b~charg  b~licha c~werks  c~lgort c~clabs c~ceinm  INTO TABLE  lt_mara_mch1
                                              FROM mara  AS a  INNER JOIN mch1  AS b
                                                  ON  a~matnr   = b~matnr
                                                  INNER JOIN mchb AS c
                                                  ON b~matnr  = c~matnr
                                                 AND b~charg  = c~charg
                                                 INNER JOIN t320 AS d
                                                  ON    c~werks  =  d~werks
                                                  AND   c~lgort  =  d~lgort
                                FOR ALL ENTRIES IN lt_stpo_phantom
                                             WHERE a~matnr  = lt_stpo_phantom-matnr
                                               AND a~mtart  IN ('ZFAB' ,'ZSRT')
                                               AND ( c~clabs    GT  0 OR c~ceinm GT  0 ) .


          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    SORT lt_mara_mch1  BY licha ASCENDING  clabs DESCENDING .
    IF lt_mara_mch1 IS NOT INITIAL .

      SELECT  werks  ,
              lgort  ,
              lgobe FROM t001l  INTO TABLE @DATA(lt_t001l)  FOR ALL ENTRIES IN @lt_mara_mch1  WHERE lgort  EQ @lt_mara_mch1-lgort
                                                                                               AND  werks  EQ @lt_mara_mch1-werks.
    ENDIF.
    LOOP AT lt_mara_mch1  INTO DATA(ls_mch1)  .
      IF ls_mch1-clabs LE  0 .
        " check  if atleast one entry  for that lot is already present in et_entityset
        " busness case show only where stock is avaialble for that lot  if not atleast one reocrd related to that lot should be shown
        " since sorting is done on lot and quantity  first record will always have quantity and will availble  in et_enityset
        READ TABLE  et_entityset    TRANSPORTING NO FIELDS WITH KEY  lot_no  = ls_mch1-licha .
        IF sy-subrc NE  0.
          DATA(lv_consider)  = abap_true .
        ENDIF .
      ENDIF.
      IF ls_mch1-clabs  GT  0  OR  ls_mch1-ceinm  GT  0  OR lv_consider EQ  abap_true ..
        APPEND INITIAL LINE TO  et_entityset ASSIGNING FIELD-SYMBOL(<fs_entityset>)  .

        <fs_entityset>-lot_no      =  ls_mch1-licha .
        <fs_entityset>-mfrpn       =  ls_mara-mfrpn .
        <fs_entityset>-charg       = ls_mch1-charg  .
        <fs_entityset>-wafer_size  = ls_mch1-groes .
        <fs_entityset>-output_part  = ls_mch1-matnr .   " lot material (input component)
        <fs_entityset>-fab_lot_mat  = ls_mch1-mfrpn .
        <fs_entityset>-clabs        = ls_mch1-clabs .
        <fs_entityset>-ceinm        = ls_mch1-ceinm .
        <fs_entityset>-werks        = ls_mch1-werks .
        READ TABLE lt_t001l    INTO DATA(ls_t001l)  WITH KEY lgort  = ls_mch1-lgort .
        IF sy-subrc EQ 0.
          <fs_entityset>-lot_location  =  ls_t001l-lgobe && '(' && ls_t001l-lgort  && ')' .
        ENDIF.

        " bacth charactersition of fab material to get fab lot charactersictics
        CLEAR lv_objkey.
        lv_objkey = ls_mch1-matnr.
        CALL FUNCTION 'CLAP_DDB_GET_CLASSIFICATION'
          EXPORTING
            object                 = lv_objkey
            obtab                  = 'MARA'
*           STANDARD_TYPE_ONLY     = ' '
*           OBJECT_EQ_CLASS        = ' '
*           SPRAS                  = SY-LANGU
*           ONLY_VARKLART          = ' '
*           POBTAB                 = ' '
*           DATE_OF_CHANGE         =
*           CHANGE_SERVICE_NUMBER  =
            classtype              = '023'
*           READ_ONLY              = ' '
*           CALLED_FROM_API        = ' '
*           SORT_POSNR             = ' '
*           IMPORTING
*           ERROR_STATU            =
          TABLES
            allocations            = lt_allocations
          EXCEPTIONS
            no_allocation          = 1
            foreign_lock           = 2
            system_failure         = 3
            set_aennr              = 4
            change_nr_not_exist    = 5
            date_in_past           = 6
            error_class            = 7
            error_date_restriction = 8
            error_status           = 9
            error_class_status     = 10
            OTHERS                 = 11.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
        CLEAR: lv_classnum.
        READ TABLE lt_allocations INTO DATA(ls_allocations) INDEX 1.
        IF sy-subrc EQ 0.
          lv_classnum = ls_allocations-class.
        ENDIF.
        CLEAR lv_objkey.
        lv_objkey = ls_mch1-matnr && ls_mch1-charg.
        CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
          EXPORTING
            objectkey       = lv_objkey
            objecttable     = 'MCH1'
            classnum        = lv_classnum
            classtype       = '023'
*           KEYDATE         = SY-DATUM
*           UNVALUATED_CHARS       = ' '
*           LANGUAGE        = SY-LANGU
*           OBJECTKEY_LONG  =
* IMPORTING
*           STATUS          =
*           STANDARDCLASS   =
          TABLES
            allocvaluesnum  = lt_allocvaluesnum
            allocvalueschar = lt_allocvalueschar
            allocvaluescurr = lt_allocvaluescurr
            return          = lt_return_t.
        IF lt_allocvalueschar  IS NOT INITIAL .
          READ TABLE lt_allocvalueschar INTO DATA(ls_char)  WITH KEY  charact  = 'DIETYPE'  .
          IF sy-subrc EQ 0.
            <fs_entityset>-lot_die_type  = ls_char-value_char  .
            CLEAR ls_char .                                 " batch die type
          ENDIF.


          READ TABLE lt_allocvalueschar INTO ls_char WITH KEY  charact  = 'DIEREV'  .
          IF sy-subrc EQ 0.
            <fs_entityset>-lot_die_rev  = ls_char-value_char  .                               " batch die rev
            CLEAR ls_char .
          ENDIF.

          REFRESH  : lt_allocvaluesnum[] ,
                     lt_allocvalueschar[] ,
                     lt_allocvaluescurr[] ,
                     lt_return_t[].
        ENDIF .
        " owner code
        CALL METHOD zcl_zebr_form_dpc_ext=>get_batch_char
          EXPORTING
            iv_matnr           = ls_mch1-matnr
            iv_charg           = ls_mch1-charg
          IMPORTING
            et_allocvaluesnum  = lt_allocvaluesnum
            et_allocvalueschar = lt_allocvalueschar
            et_allocvaluescurr = lt_allocvaluescurr
            et_return          = lt_return_t.
        READ TABLE lt_allocvalueschar INTO ls_char WITH KEY  charact  = 'OWNER_CODE' .
        IF sy-subrc EQ 0.
          <fs_entityset>-owner_code  = ls_char-value_char  .
          CLEAR ls_char .
        ENDIF.


        CLEAR : ls_mch1 .
        REFRESH  : lt_allocvaluesnum[] ,
                   lt_allocvalueschar[] ,
                   lt_allocvaluescurr[] ,
                   lt_return_t[].
      ENDIF.
      CLEAR lv_consider .
    ENDLOOP .
    SORT et_entityset BY lot_no lot_die_type  lot_die_rev .
    DELETE ADJACENT DUPLICATES FROM et_entityset COMPARING lot_no  lot_die_type  lot_die_rev  .
*
    CLEAR : lv_mfrpn ,
            lv_matnr_op .

  ENDMETHOD.


  METHOD EBRMASTERSTATUSS_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRMASTERSTATUSS_GET_ENTITYSET
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
    DATA : lrt_reqno             TYPE RANGE OF zebr_de_req_no,
           lv_ov_gts_clfs        TYPE char01,
           lv_ov_material_status TYPE char01,
           lv_ov_pv_status       TYPE char01,
           lv_ov_route_status    TYPE char01,
           lv_ov_bom_status      TYPE char01.


    LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options)  .
      IF ls_filter_select_options-property  = 'ReqNo' .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_reqno) INDEX 1 .
        IF sy-subrc EQ  0 .
          TRANSLATE  ls_filter_reqno-low  TO UPPER CASE .
          TRANSLATE  ls_filter_reqno-high  TO UPPER CASE .
          APPEND INITIAL LINE TO lrt_reqno ASSIGNING FIELD-SYMBOL(<fr_reqno>).
          <fr_reqno>-sign  =  ls_filter_reqno-sign .
          <fr_reqno>-option = ls_filter_reqno-option  .
          <fr_reqno>-low = ls_filter_reqno-low  .
          <fr_reqno>-high = ls_filter_reqno-high  .
        ENDIF.
      ENDIF.
    ENDLOOP.

    SELECT  a~req_no        ,
            a~output_matnr ,
            a~lot_no        ,
            a~matnr        ,
            a~bom_no        ,
            a~bom_alt        ,
            a~route_number        ,
            a~route_counter        ,
            a~material_status        ,
            a~bom_status        ,
            a~route_status        ,
            a~pv_status        ,
            a~gts_clfs        ,
            a~prod_version     ,
            b~mtart  FROM zebr_t_mast_data  AS  a INNER JOIN  mara AS b
                               ON a~matnr  = b~matnr
                 INTO TABLE @DATA(lt_master)  WHERE req_no  IN @lrt_reqno .

    " overall material status

    LOOP AT  lt_master  INTO DATA(ls_master)  WHERE  material_status =   'C' OR
                                                     material_status =   'W1' OR
                                                     material_status =   'W3' OR
                                                     material_status =   'W4' OR
                                                     material_status =   'X0'  .
      lv_ov_material_status  =  'R'   .
      IF lv_ov_material_status  IS NOT INITIAL .
        EXIT .
      ENDIF.
    ENDLOOP.
    IF lv_ov_material_status IS INITIAL .
      LOOP AT  lt_master  INTO ls_master  WHERE  material_status =   'P' OR
                                                 material_status =   'Q' OR
                                                 material_status =   'W2'  .
        lv_ov_material_status  =  'Y'   .
        IF lv_ov_material_status  IS NOT INITIAL .
          EXIT .
        ENDIF.
      ENDLOOP.
      IF lv_ov_material_status IS INITIAL .
        LOOP AT  lt_master  INTO ls_master  WHERE  material_status =   'R1' OR
                                                   material_status =   'R2'   .
          lv_ov_material_status  =  'G'   .
          IF lv_ov_material_status  IS NOT INITIAL .
            EXIT .
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    " bom status
    LOOP AT  lt_master  INTO ls_master  WHERE  bom_status =   '01' OR
                                                     bom_status =   '04'   .
      lv_ov_bom_status  =  'R'   .
      IF lv_ov_bom_status  IS NOT INITIAL .
        EXIT .
      ENDIF.
    ENDLOOP.
    IF lv_ov_bom_status IS INITIAL .
      READ TABLE lt_master   INTO ls_master  WITH KEY  bom_status = '02'  .
      IF sy-subrc EQ 0 .
        lv_ov_bom_status  =  'Y'  .
      ELSE.
        READ TABLE lt_master   INTO ls_master  WITH KEY  bom_status = '03'  .
        IF sy-subrc EQ 0 .
          lv_ov_bom_status  =  'G'  .
        ENDIF.
      ENDIF.
    ENDIF.
    " Route status
    LOOP AT  lt_master  INTO ls_master  WHERE  bom_status =   '01' OR
                                                     bom_status =   '05'   .
      lv_ov_route_status  =  'R'   .
      IF lv_ov_route_status  IS NOT INITIAL .
        EXIT .
      ENDIF.
    ENDLOOP.
    IF lv_ov_route_status IS INITIAL .
      READ TABLE lt_master   INTO ls_master  WITH KEY  bom_status = '04'  .
      IF sy-subrc EQ 0 .
        lv_ov_route_status  =  'G'  .
      ENDIF.
    ENDIF.

    " pv status
    READ TABLE lt_master  INTO ls_master  WITH KEY  prod_version  =  ' '  .
    IF  sy-subrc EQ 0.
      lv_ov_pv_status    = 'R'  .
    ELSE.
      lv_ov_pv_status    = 'G'  .
    ENDIF.

    " gts status
    READ TABLE  lt_master INTO ls_master  WITH KEY  gts_clfs  =  ' ' .
    IF  sy-subrc EQ 0.
      lv_ov_gts_clfs    = 'R'  .
    ELSE.
      lv_ov_gts_clfs    = 'G'  .
    ENDIF.

    " mark overall status as 'R when any one status are R
    IF lv_ov_gts_clfs EQ 'R'  OR lv_ov_material_status   EQ 'R'  OR
        lv_ov_bom_status  EQ  'R'  OR  lv_ov_pv_status   EQ 'R'   OR lv_ov_route_status  EQ 'R'  .
      DATA(lv_ov_master_status)   =  'R'.
    ELSE.
      IF  lv_ov_material_status   EQ 'Y'  OR
     lv_ov_bom_status  EQ  'Y'  OR lv_ov_route_status  EQ 'Y'  .
        lv_ov_master_status   =  'Y'  .
      ELSE.
        lv_ov_master_status   =  'G'  .
      ENDIF.
    ENDIF.



    LOOP AT lt_master  INTO ls_master .
      APPEND INITIAL LINE TO  et_entityset  ASSIGNING FIELD-SYMBOL(<fs_mastdata>).
      <fs_mastdata>-req_no            =  ls_master-req_no .
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          input  = ls_master-matnr
        IMPORTING
          output = <fs_mastdata>-matnr.

       CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          input  = ls_master-output_matnr
        IMPORTING
          output = <fs_mastdata>-output_matnr.

      <fs_mastdata>-lot_no            =  ls_master-lot_no  .
      <fs_mastdata>-gts_clfs          =  ls_master-gts_clfs        .
      <fs_mastdata>-material_status   =  ls_master-material_status      .
      <fs_mastdata>-pv_status         =  ls_master-pv_status        .
      <fs_mastdata>-route_status      =  ls_master-route_status      .
      <fs_mastdata>-bom_status        =  ls_master-bom_status      .
      <fs_mastdata>-ov_gts_clfs          =    lv_ov_gts_clfs         .
      <fs_mastdata>-ov_material_status   =    lv_ov_material_status         .
      <fs_mastdata>-ov_pv_status         =    lv_ov_pv_status       .
      <fs_mastdata>-ov_route_status      =    lv_ov_route_status       .
      <fs_mastdata>-ov_bom_status        =    lv_ov_bom_status       .

      IF ls_master-mtart EQ  'FERT'  .
        <fs_mastdata>-seq  =  '1'  .
      ELSEIF    ls_master-mtart EQ  'ZTST'    .
        <fs_mastdata>-seq  =  '2'  .
      ELSEIF    ls_master-mtart EQ  'ZPBS'     .
        <fs_mastdata>-seq  =  '3'  .
      ELSEIF    ls_master-mtart EQ  'ZBMP'    .
        <fs_mastdata>-seq  =  '4'  .
      ELSEIF    ls_master-mtart EQ  'ZASY'     .
        <fs_mastdata>-seq  =  '5'  .
      ELSEIF    ls_master-mtart EQ  'ZSRT'    .
        <fs_mastdata>-seq  =  '6'  .
      ELSEIF    ls_master-mtart EQ  'ZFAB'    .
        <fs_mastdata>-seq  =  '7'  .
      ENDIF.

    ENDLOOP.
    SORT et_entityset  BY req_no output_matnr matnr .
    DELETE ADJACENT DUPLICATES FROM et_entityset    COMPARING  req_no output_matnr matnr .
    SORT  et_entityset  BY  req_no output_matnr seq ASCENDING .

  ENDMETHOD.


  method EBRMATSEARCHSET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->EBRMATSEARCHSET_GET_ENTITY
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
  endmethod.


  METHOD EBRMATSEARCHSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRMATSEARCHSET_GET_ENTITYSET
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
*ENDTRY.
    DATA  : lrt_mtart      TYPE RANGE OF mtart,
            lrt_mfrpn      TYPE RANGE OF mfrpn,
            lrt_bismt      TYPE RANGE OF bismt,
            lrt_searchterm TYPE RANGE OF  char40.


    LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options)  .
      IF ls_filter_select_options-property  = 'Bismt' .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_bismt) INDEX 1 .
        IF sy-subrc EQ  0 .
         if ls_filter_bismt-low   is NOT INITIAL .
          TRANSLATE  ls_filter_bismt-low  TO UPPER CASE .
          TRANSLATE  ls_filter_bismt-high  TO UPPER CASE .
          APPEND INITIAL LINE TO lrt_bismt ASSIGNING FIELD-SYMBOL(<fr_bismt>).
          <fr_bismt>-sign  =  ls_filter_bismt-sign .
          <fr_bismt>-option = ls_filter_bismt-option  .
          <fr_bismt>-low = ls_filter_bismt-low  .
          <fr_bismt>-high = ls_filter_bismt-high  .
          endif.
        ENDIF.
      ELSEIF ls_filter_select_options-property  = 'Mtart' .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_mtart) INDEX 1 .
        IF sy-subrc EQ  0 .
          if ls_filter_mtart-low   is NOT INITIAL .
          TRANSLATE ls_filter_mtart-low TO UPPER CASE .
          TRANSLATE ls_filter_mtart-high TO UPPER CASE .
          APPEND INITIAL LINE TO lrt_mtart ASSIGNING FIELD-SYMBOL(<fr_mtart>).
          <fr_mtart>-sign  =  ls_filter_mtart-sign .
          <fr_mtart>-option = ls_filter_mtart-option  .
          <fr_mtart>-low = ls_filter_mtart-low  .
          <fr_mtart>-high = ls_filter_mtart-high  .
          endif.
        ENDIF.
      ELSEIF  ls_filter_select_options-property   = 'Mfrpn' .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_mfrpn) INDEX 1 .
        IF sy-subrc EQ  0 .
          if ls_filter_mfrpn  is NOT INITIAL .
          TRANSLATE ls_filter_mfrpn-low TO UPPER CASE .
          TRANSLATE ls_filter_mfrpn-high TO UPPER CASE .
          APPEND INITIAL LINE TO lrt_mfrpn ASSIGNING FIELD-SYMBOL(<fr_mfrpn>).
          <fr_mfrpn>-sign   = ls_filter_mfrpn-sign .
          <fr_mfrpn>-option = ls_filter_mfrpn-option  .
          <fr_mfrpn>-low    = ls_filter_mfrpn-low  .
          <fr_mfrpn>-high   = ls_filter_mfrpn-high  .
          endif.
        ENDIF.
      ELSEIF  ls_filter_select_options-property   = 'Searchterm' .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_searchterm) INDEX 1 .
        IF sy-subrc EQ  0 .
          TRANSLATE ls_filter_searchterm-low TO UPPER CASE .
          TRANSLATE ls_filter_searchterm-high TO UPPER CASE .
          APPEND INITIAL LINE TO lrt_searchterm ASSIGNING FIELD-SYMBOL(<fr_searchterm>) .
          <fr_searchterm>-sign    = ls_filter_searchterm-sign .
          <fr_searchterm>-option  = ls_filter_searchterm-option  .
          <fr_searchterm>-low     = ls_filter_searchterm-low  .
          <fr_searchterm>-high    = ls_filter_searchterm-high  .

          APPEND INITIAL LINE TO lrt_mfrpn ASSIGNING <fr_mfrpn>.
          <fr_mfrpn>-sign    = ls_filter_searchterm-sign .
          <fr_mfrpn>-option  = ls_filter_searchterm-option  .
          <fr_mfrpn>-low     = ls_filter_searchterm-low  .
          <fr_mfrpn>-high    = ls_filter_searchterm-high  .

          APPEND INITIAL LINE TO lrt_mtart ASSIGNING <fr_mtart>.
          <fr_mtart>-sign   = ls_filter_searchterm-sign .
          <fr_mtart>-option = ls_filter_searchterm-option  .
          <fr_mtart>-low    = ls_filter_searchterm-low  .
          <fr_mtart>-high   = ls_filter_searchterm-high  .

          APPEND INITIAL LINE TO lrt_bismt ASSIGNING <fr_bismt> .
          <fr_bismt>-sign     = ls_filter_searchterm-sign .
          <fr_bismt>-option   = ls_filter_searchterm-option  .
          <fr_bismt>-low      = ls_filter_searchterm-low  .
          <fr_bismt>-high     = ls_filter_searchterm-high  .
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF lrt_searchterm IS NOT INITIAL .

      IF  is_paging-top > 0 .
        DATA(lv_rowcount)  =  is_paging-top  + is_paging-skip.
        SELECT mtart
               bismt
               mfrpn   FROM mara INTO TABLE et_entityset UP TO lv_rowcount ROWS  WHERE bismt IN lrt_bismt "mtart in lrt_mtart
                                                          OR mfrpn  IN lrt_mfrpn
                                                          OR mtart  IN lrt_mtart
                                                         .
      ELSE.
        SELECT mtart
           bismt
           mfrpn   FROM mara INTO TABLE et_entityset  WHERE bismt IN lrt_bismt "mtart in lrt_mtart
                                                      OR mfrpn  IN lrt_mfrpn
                                                      OR mtart  IN lrt_mtart .
      ENDIF.

    ELSE.
      IF lrt_mtart IS NOT INITIAL .
        IF  is_paging-top > 0 .
          lv_rowcount  =  is_paging-top  + is_paging-skip.
          SELECT mtart
            bismt
            mfrpn   FROM mara INTO TABLE et_entityset UP TO lv_rowcount ROWS
                                           WHERE  bismt IN    lrt_bismt
                                           AND    mfrpn  IN lrt_mfrpn
                                            AND   mtart  IN lrt_mtart  "mtart in lrt_mtart
              .                                        .
        ELSE.
          SELECT mtart
                  bismt
                  mfrpn   FROM mara INTO TABLE et_entityset  WHERE bismt IN lrt_bismt
                                                             AND   mfrpn  IN lrt_mfrpn
                                                             AND   mtart  IN lrt_mtart .   "mtart in lrt_mtart
        ENDIF.
      ELSE.
        IF  is_paging-top > 0 .
          lv_rowcount  =  is_paging-top  + is_paging-skip.
          SELECT mtart
               bismt
               mfrpn   FROM mara INTO TABLE et_entityset UP TO lv_rowcount ROWS
                                                        WHERE bismt  IN lrt_bismt
                                                          AND   mfrpn  IN lrt_mfrpn
                                                          AND   mtart  IN ('FERT','ZTST','ZASY','ZBMP','ZPBS')  "mtart in lrt_mtart
                                                          .

        ELSE.
          SELECT mtart
              bismt
              mfrpn   FROM mara INTO TABLE et_entityset
                                                        WHERE bismt  IN lrt_bismt
                                                         AND   mfrpn  IN lrt_mfrpn
                                                         AND   mtart  IN ('FERT','ZTST','ZASY','ZBMP','ZPBS') .  "mtart in lrt_mtart


        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR : lv_rowcount.

    CALL METHOD /iwbep/cl_mgw_data_util=>paging
      EXPORTING
        is_paging = is_paging
      CHANGING
        ct_data   = et_entityset.
  ENDMETHOD.


  METHOD EBRMFRPNSET_GET_ENTITY.
    DATA : lv_mfrpn     TYPE mfrpn,
           lv_subreq    TYPE zcl_zebr_form_mpc_ext=>ts_ebrmfrpn-subreq_val,
           lv_mpw       TYPE zcl_zebr_form_mpc_ext=>ts_ebrmfrpn-mpw,
           lv_hyb       TYPE zcl_zebr_form_mpc_ext=>ts_ebrmfrpn-hybrid,
           lo_exception TYPE REF TO /iwbep/cx_mgw_tech_exception,
           lt_stpo      TYPE ztt_bom_com_level,
           lv_msg_txt   TYPE bapi_msg.
    CREATE OBJECT lo_exception.
    READ TABLE it_key_tab INTO DATA(ls_keytab)  WITH KEY name  = 'Mfrpn'  .
    IF sy-subrc  EQ 0 .

      lv_mfrpn  =  ls_keytab-value .
      READ TABLE it_key_tab INTO DATA(ls_keytab_req)  WITH KEY name  = 'Subreq'  .
      IF sy-subrc EQ 0.
        lv_subreq   =  ls_keytab_req-value .
      ENDIF.

      READ TABLE it_key_tab INTO DATA(ls_keytab_mpw)  WITH KEY name  = 'Mpw'  .
      IF sy-subrc EQ 0.
        lv_mpw   =  ls_keytab_mpw-value .
      ENDIF.

      READ TABLE it_key_tab INTO DATA(ls_keytab_hyb)  WITH KEY name  = 'Hybrid'  .
      IF sy-subrc EQ 0.
        lv_hyb   =  ls_keytab_hyb-value .
      ENDIF.

*  CALL METHOD cl_http_utility=>get_last_error
*    receiving
*      rc     = v_return_code.

      SELECT  SINGLE matnr  , mtart, mfrpn   INTO @DATA(ls_mara) FROM mara WHERE mfrpn  = @lv_mfrpn .
      IF sy-subrc NE 0.
        CALL METHOD cl_http_utility=>unescape_url
          EXPORTING
            escaped   = ls_keytab-value
          RECEIVING
            unescaped = lv_mfrpn.
        er_entity-mfrpn  =  lv_mfrpn .
        SELECT  SINGLE matnr  mtart  mfrpn   INTO ls_mara FROM mara WHERE mfrpn  = lv_mfrpn .
        IF sy-subrc  NE  0  .

          er_entity-msg_stat   = 'E'  .
          IF  lo_exception IS BOUND  .
            CONCATENATE  'Material '  lv_mfrpn    ' is Invalid'   INTO lv_msg_txt  RESPECTING BLANKS .
            lo_exception->get_msg_container( )->add_message(
                   EXPORTING
                     iv_msg_type    = 'E'
                     iv_msg_id      = '00'
                     iv_msg_number  = '123'
                     iv_msg_text    = lv_msg_txt ).
            RAISE EXCEPTION lo_exception.
            RETURN.
            CLEAR lv_msg_txt .
          ENDIF.
        ELSE.
*          IF ( lv_subreq  =  'PRQ'    OR  lv_subreq = 'ABO'  ) .
*            IF ( ls_mara-mtart  =  'ZASY'  OR  ls_mara-mtart  =  'ZBMP'  )  .
*              er_entity-mfrpn  =  lv_mfrpn .
*              er_entity-msg_stat   = 'S'  .
*            ELSE .
*              IF  lo_exception IS BOUND  .
*                er_entity-msg_stat   = 'E'  .
*                lo_exception->get_msg_container( )->add_message(
*                    EXPORTING
*                      iv_msg_type     = 'E'
*                      iv_msg_id      = '00'
*                      iv_msg_number  = '123'
*                      iv_msg_text    = 'Material ' &&  lv_mfrpn  &&  '  should be Assembly or Bump' ).
*                RAISE EXCEPTION lo_exception.
*                RETURN.
*              ENDIF.
*            ENDIF.
*
*          ELSE  .


          er_entity-mfrpn  =  lv_mfrpn .
          er_entity-msg_stat   = 'S'  .
*          ENDIF.
        ENDIF.

      ELSE.
*        IF ( lv_subreq  =  'PRQ'    OR  lv_subreq = 'ABO'  ) .
*          IF ( ls_mara-mtart  =  'ZASY'  OR  ls_mara-mtart  =  'ZBMP'  )  .
*            er_entity-mfrpn  =  lv_mfrpn .
*            er_entity-msg_stat   = 'S'  .
*          ELSE .
*            IF  lo_exception IS BOUND  .
*              er_entity-msg_stat   = 'E'  .
*              lo_exception->get_msg_container( )->add_message(
*                  EXPORTING
*                    iv_msg_type     = 'E'
*                    iv_msg_id      = '00'
*                    iv_msg_number  = '123'
*                    iv_msg_text    = 'Material ' &&  lv_mfrpn  &&  '  should be Assembly or Bump' ).
*              RAISE EXCEPTION lo_exception.
*              RETURN.
*            ENDIF.
*          ENDIF.
*
*        ELSE  .

        er_entity-mfrpn  =  lv_mfrpn .
        er_entity-msg_stat   = 'S'  .
*        ENDIF.
      ENDIF.
      IF ( er_entity-mfrpn  =  lv_mfrpn  )  AND (    er_entity-msg_stat   = 'S' )  AND lv_mpw  NE 'X'  .
        SELECT  SINGLE matnr  INTO @DATA(lv_matnr)   FROM mara WHERE mfrpn  = @lv_mfrpn .
        CALL METHOD zcl_bom_explode_util=>bom_explode
          EXPORTING
            iv_matnr              = lv_matnr
            iv_werks              = '4900'
*           ls_stpo               =
            iv_get_marterial_type = 'X'
            iv_active_bom_01      = 'X'
*           IV_USAGE_PROBABILITY  = 'X'
          CHANGING
            it_stpo               = lt_stpo.

        IF lt_stpo IS NOT INITIAL .
          DELETE lt_stpo WHERE  zdie_type  IS INITIAL .
          DELETE lt_stpo WHERE  zdie_revcode  IS INITIAL .
          READ TABLE  lt_stpo    INTO DATA(ls_stpo)  INDEX 1 .
          IF sy-subrc  EQ 0 .

            SELECT  SINGLE pdietype , pdierev   , counter   INTO  @DATA(ls_phantom)
                                                        FROM zmm_phantom_die  WHERE   pdietype = @ls_stpo-zdie_type
                                                                                AND   pdierev  = @ls_stpo-zdie_revcode .
            IF sy-subrc  EQ 0 .
              IF ls_phantom-counter   GE 1   AND lv_hyb  NE 'X'  .
                er_entity-msg_stat   = 'E'  .
                IF  lo_exception IS BOUND  .
                  CONCATENATE  'Select Hybrid  switch for Material '  lv_mfrpn   INTO lv_msg_txt  RESPECTING BLANKS .
                  lo_exception->get_msg_container( )->add_message(
                         EXPORTING
                           iv_msg_type     = 'E'
                           iv_msg_id      = '00'
                           iv_msg_number  = '123'
                           iv_msg_text    = lv_msg_txt  ).
                  RAISE EXCEPTION lo_exception.
                  RETURN.
                  CLEAR lv_msg_txt .
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            IF lv_hyb IS  NOT INITIAL .
              er_entity-msg_stat   = 'E'  .
              CONCATENATE  'Material :   '   lv_mfrpn    ' is not a hybrid material'  INTO lv_msg_txt  RESPECTING BLANKS .
              IF  lo_exception IS BOUND  .
                lo_exception->get_msg_container( )->add_message(
                       EXPORTING
                         iv_msg_type     = 'E'
                         iv_msg_id      = '00'
                         iv_msg_number  = '123'
                         iv_msg_text    = lv_msg_txt  ).
                RAISE EXCEPTION lo_exception.
                RETURN.
              ENDIF.
              CLEAR   lv_msg_txt.
            ENDIF.
          ENDIF.


        ENDIF.
      ENDIF.
    ELSE.
    ENDIF.
  ENDMETHOD.


  method EBRNOTIFICATIONS_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRNOTIFICATIONS_GET_ENTITYSET
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
  endmethod.


  METHOD EBRNPPDETSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRNPPDETSET_GET_ENTITYSET
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
    DATA : lrt_reqno             TYPE RANGE OF zebr_de_req_no.

    LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options)  .
      IF ls_filter_select_options-property  = 'ReqNo' .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_reqno) INDEX 1 .
        IF sy-subrc EQ  0 .
          TRANSLATE  ls_filter_reqno-low  TO UPPER CASE .
          TRANSLATE  ls_filter_reqno-high  TO UPPER CASE .
          APPEND INITIAL LINE TO lrt_reqno ASSIGNING FIELD-SYMBOL(<fr_reqno>).
          <fr_reqno>-sign  =  ls_filter_reqno-sign .
          <fr_reqno>-option = ls_filter_reqno-option  .
          <fr_reqno>-low = ls_filter_reqno-low  .
          <fr_reqno>-high = ls_filter_reqno-high  .
        ENDIF.
      ENDIF.
    ENDLOOP.

    SELECT req_no  ,planning_id FROM zebr_t_lotinfo  INTO TABLE @DATA(lt_planid)  WHERE  req_no   IN @lrt_reqno                   "#EC CI_NO_TRANSFORM
                                                                                   AND   planning_id NE ' '.
    IF sy-subrc EQ 0.
      SELECT aufnr , planning_id ,lot_number ,datum , ship_from , ship_to ,input_primary_qty, output_matnr
        FROM zpp_build_instr INTO TABLE @DATA(lt_zpp_instr)
          FOR ALL ENTRIES IN  @lt_planid                "#EC CI_NO_TRANSFORM
        WHERE planning_id  EQ @lt_planid-planning_id  . "#EC CI_NOFIRST
      IF sy-subrc EQ   0 .
        SELECT  aufnr , gltrp, gstrp  FROM afko INTO TABLE @DATA(lt_afko) FOR ALL ENTRIES IN  @lt_zpp_instr WHERE  aufnr   =  @lt_zpp_instr-aufnr .  "#EC CI_NO_TRANSFORM
        LOOP AT lt_zpp_instr INTO DATA(ls_zpp_instr)  .

          APPEND INITIAL LINE TO  et_entityset ASSIGNING FIELD-SYMBOL(<fs_nppdet>)  .
          READ TABLE  lt_planid  INTO DATA(ls_planid)  WITH KEY  planning_id  =  ls_zpp_instr-planning_id .
          IF sy-subrc EQ 0  .
            <fs_nppdet>-req_no    =  ls_planid-req_no   .
*    <fs_nppdet>-asy_date  =  sy-datum .
*    <fs_nppdet>-wf_date   =  sy-datum .
*    <fs_nppdet>-ast_sod   =  sy-datum .
            <fs_nppdet>-plan_id   =  ls_zpp_instr-planning_id .
            <fs_nppdet>-ship_loc  =  ls_zpp_instr-ship_to .
*    <fs_nppdet>-ct_commit =  'X'  .
            <fs_nppdet>-asy_lot   =  ls_zpp_instr-lot_number .
            <fs_nppdet>-qty       =  ls_zpp_instr-input_primary_qty .
            <fs_nppdet>-br_issue  =  ls_zpp_instr-datum.
            <fs_nppdet>-part_numbe  =  ls_zpp_instr-output_matnr.
            READ TABLE  lt_afko INTO DATA(ls_afko)  WITH KEY aufnr  = ls_zpp_instr-aufnr .
            IF sy-subrc EQ 0.
              <fs_nppdet>-commited_date   =  ls_afko-gltrp.
              <fs_nppdet>-asy_date        =  ls_afko-gstrp.
            ENDIF.
            CLEAR  ls_planid .
          ENDIF .
          CLEAR  ls_zpp_instr .
        ENDLOOP .

      ELSE .
        SELECT planning_id , final_ship_to, output_material FROM zscm_load_plan  INTO TABLE @DATA(lt_load_plan)
                     FOR ALL ENTRIES IN @lt_planid  WHERE planning_id  = @lt_planid-planning_id . "#EC CI_NOFIRST
        IF sy-subrc  EQ 0 .
          LOOP AT  lt_load_plan  INTO DATA(ls_load_plan) .
            APPEND INITIAL LINE TO  et_entityset ASSIGNING <fs_nppdet> .
            READ TABLE  lt_planid  INTO ls_planid  WITH KEY  planning_id  =  ls_load_plan-planning_id .
            IF sy-subrc EQ 0   .
              <fs_nppdet>-req_no    =  ls_planid-req_no.
*              <fs_nppdet>-asy_date  =  sy-datum .
*              <fs_nppdet>-wf_date   =  sy-datum .
*              <fs_nppdet>-ast_sod   =  sy-datum .
              <fs_nppdet>-plan_id   =   ls_load_plan-planning_id.
              <fs_nppdet>-ship_loc  =   ls_load_plan-final_ship_to.
              <fs_nppdet>-part_numbe  =   ls_load_plan-output_material.
*              <fs_nppdet>-ct_commit =  'X'  .
*              <fs_nppdet>-asy_lot   =  'LOT123' .
*              <fs_nppdet>-qty       =  '12' .
*              <fs_nppdet>-br_issue  =  sy-datum.
              CLEAR  ls_planid .
            ENDIF .
            CLEAR  ls_load_plan.

          ENDLOOP.

        ENDIF.
      ENDIF.
    ENDIF.
    if et_entityset is NOT INITIAL .
      sort et_entityset by  Req_No
                            Plan_Id
                            Asy_Lot.
      DELETE ADJACENT DUPLICATES FROM et_entityset  COMPARING   Req_No
                                                                Plan_Id
                                                                Asy_Lot.

    endif.


  ENDMETHOD.


  METHOD EBRPARTINFOSET_CREATE_ENTITY.
*    DATA: er_entity TYPE zcl_zebr_form_mpc=>ts_ebrpartinfo,
*          ls_header      TYPE zebr_t_header,
*          lv_reqno       TYPE  zebr_t_header-req_no,
*          lo_exception   TYPE REF TO /iwbep/cx_mgw_tech_exception,
*          lv_matnr       TYPE matnr,
*          lt_num         TYPE TABLE OF bapi1003_alloc_values_num,
*          lt_char        TYPE TABLE OF bapi1003_alloc_values_char,
*          lt_curr        TYPE TABLE OF bapi1003_alloc_values_curr,
*          lt_return      TYPE TABLE OF bapiret2,
*          lv_objkey      TYPE objnum,
*          lt_value_char  TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
*          lv_fert_matnr  TYPE matnr,
*          lv_out_matnr   TYPE matnr,
*          lt_bom_details TYPE ztt_bom_item,
*          lt_bom_output  TYPE ztt_bom_ip_out.
*
*
*    CLEAR: lv_reqno,er_entity,ls_header,
*           er_entity  , ls_header  , lv_reqno ,
*           lo_exception    , lv_matnr   , lt_num,lt_char ,
*           lt_return       , lv_objkey  , lv_fert_matnr ,lt_curr .
*
*    CREATE OBJECT lo_exception.
*    io_data_provider->read_entry_data(
*   IMPORTING
*   es_data = er_entity ).
*
*    .
*    " get define product line ID and description
*    SELECT de_prod_line_id ,
*           prod_line_id ,
*           de_prod_lin_name FROM zde_prod_line INTO TABLE  @DATA(lt_zde_prod_line)
*                                                    WHERE de_prod_line_id IS NOT NULL.
*    IF sy-subrc EQ  0.
*      LOOP AT  lt_zde_prod_line ASSIGNING FIELD-SYMBOL(<fs_de_prod_line>).
*        TRANSLATE <fs_de_prod_line>-de_prod_lin_name TO UPPER CASE .
*      ENDLOOP.
*    ENDIF.
*
*    IF er_entity IS INITIAL AND lo_exception IS BOUND.
*      lo_exception->get_msg_container( )->add_message(
*         EXPORTING
*           iv_msg_type     = 'E'
*           iv_msg_id      = '00'
*           iv_msg_number  = '123'
*           iv_msg_text    = 'No input received to backend' ).
**          IV_MSG_V1
*      RAISE EXCEPTION lo_exception.
*      RETURN.
*    ENDIF.
*    IF er_entity-output_part  IS NOT INITIAL .
*      TRANSLATE er_entity-output_part TO UPPER CASE.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = er_entity-output_part
*        IMPORTING
*          output = lv_out_matnr.
*      " find the highest material
*      " get materil till we get fert .
*      CALL FUNCTION 'ZGET_MULTILEVEL_BOM_BOTTOM_UP'
*        EXPORTING
*          im_matnr         = lv_out_matnr
*        CHANGING
*          ct_bom_details   = lt_bom_details
*          ct_bom_input_out = lt_bom_output.
*      DELETE lt_bom_details  WHERE zout_mtart NE 'FERT'  .
*      READ TABLE lt_bom_details  INTO DATA(ls_bom_details) INDEX 1  .
*      IF sy-subrc EQ  0.
*        SELECT SINGLE matnr FROM mara INTO @DATA(lv_matnr_tmp) WHERE matnr EQ @ls_bom_details-zout_mat.
*        IF sy-subrc EQ 0.
*          " get the fert material to use further  for find characteristics
*          lv_fert_matnr   = ls_bom_details-zout_mtart .
*        ELSE.
*          lo_exception->get_msg_container( )->add_message(
*               EXPORTING
*                 iv_msg_type     = 'E'
*                 iv_msg_id      = '00'
*                 iv_msg_number  = '123'
*                 iv_msg_text    = 'Output Material' && ` ` && er_entity-output_part && ` ` && 'does not exist' ).
*          RAISE EXCEPTION lo_exception.
*          RETURN.
*        ENDIF.
*      ELSE.
*        lo_exception->get_msg_container( )->add_message(
*              EXPORTING
*                iv_msg_type     = 'E'
*                iv_msg_id      = '00'
*                iv_msg_number  = '123'
*                iv_msg_text    = 'Output Material' && ` ` && er_entity-output_part && ` ` && 'does not exist' ).
*        RAISE EXCEPTION lo_exception.
*        RETURN.
*      ENDIF.
*    ENDIF.
*    CLEAR  lv_objkey.
*    lv_objkey =   lv_fert_matnr.
*    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
*      EXPORTING
*        objectkey       = lv_objkey
*        objecttable     = 'MARA'
*        classnum        = 'BU_RESPONSIBILITY'
*        classtype       = '001'
*        keydate         = sy-datum
*        language        = sy-langu
*      TABLES
*        allocvaluesnum  = lt_num
*        allocvalueschar = lt_char
*        allocvaluescurr = lt_curr
*        return          = lt_return.
*
*    APPEND LINES OF lt_char TO lt_value_char .
*    REFRESH lt_char[].
*
*    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
*      EXPORTING
*        objectkey       = lv_objkey
*        objecttable     = 'MARA'
*        classnum        = 'IC_CHARACTERISTICS'
*        classtype       = '001'
*        keydate         = sy-datum
*        language        = sy-langu
*      TABLES
*        allocvaluesnum  = lt_num
*        allocvalueschar = lt_char
*        allocvaluescurr = lt_curr
*        return          = lt_return.
*
*    APPEND LINES OF lt_char TO lt_value_char .
*    REFRESH lt_char[] .
*
*    " package code
*    READ TABLE lt_value_char INTO DATA(ls_valuechar) WITH KEY charact  = 'PKGCODE'.
*    IF sy-subrc EQ 0  .
*      er_entity-package_code   = ls_valuechar-value_char  .
*    ENDIF.
*
*    " package type
*    READ TABLE lt_value_char INTO ls_valuechar WITH KEY charact  = 'PKGTYPE'.
*    IF sy-subrc EQ 0  .
*      er_entity-package_type   = ls_valuechar-value_char  .
*    ENDIF.
*
*    " detailed product line
*    READ TABLE lt_value_char INTO ls_valuechar WITH KEY charact = 'PRODLINEDET' .
*    IF sy-subrc EQ  0 .
*      TRANSLATE ls_valuechar-value_char TO UPPER CASE.
*      READ TABLE lt_zde_prod_line INTO DATA(ls_zde_prod_line)  WITH KEY de_prod_lin_name = ls_valuechar-value_char .
*      IF sy-subrc EQ 0 .
*        DATA(lv_dpl)   =   ls_zde_prod_line-de_prod_line_id .
*      ENDIF.
*
*      SELECT SINGLE prod_line_id
*        FROM zde_prod_line
*         INTO @DATA(lv_product_line)
*         WHERE de_prod_line_id = @lv_dpl.
*
*      " buinses unit
*      SELECT SINGLE bu_id
*        FROM zbucp_prod_line
*        INTO @DATA(lv_business_unit)  WHERE product_line_id = @lv_product_line.
*
*      " bu name
*      SELECT SINGLE bu_unit_name
*        FROM zbucp_busin_unit
*        INTO @DATA(lv_bu_unit_name)
*        WHERE bu_id = @lv_business_unit.
*      IF sy-subrc EQ 0.
*        er_entity-bu_name    = lv_bu_unit_name .
*      ENDIF.
*    ENDIF.
*
*    " pass data to entity back
*    MOVE-CORRESPONDING er_entity   TO er_entity .
*    CLEAR : lv_fert_matnr ,
*            lv_objkey     ,
*            lt_num[]      ,
*            lt_char[]     ,
*            lt_curr[]     ,
*            lt_return[]   ,
*            lt_value_char[],
*            er_entity ,
*            ls_header      ,
*            lv_reqno       ,
*            lo_exception   ,
*            lv_matnr       ,
*            lt_num         ,
*            lt_char        ,
*            lt_curr        ,
*            lt_return      ,
*            lv_objkey      ,
*            lt_value_char  ,
*            lv_fert_matnr  ,
*            lv_out_matnr   ,
*            lt_bom_details ,
*            lt_bom_output  .





            .
  ENDMETHOD.


  METHOD EBRPARTINFOSET_GET_ENTITY.

    TYPES: ts_stpo_level  TYPE  zbom_com_level,
           tty_stpo_level TYPE ztt_bom_com_level.

    DATA : lv_output_fert TYPE matnr,
           lv_ftk         TYPE zebr_config-field2,
           ls_header      TYPE zebr_t_header,
           lv_reqno       TYPE  zebr_t_header-req_no,
           lo_exception   TYPE REF TO /iwbep/cx_mgw_tech_exception,
           lv_matnr       TYPE matnr,
           lt_num         TYPE TABLE OF bapi1003_alloc_values_num,
           lt_char        TYPE TABLE OF bapi1003_alloc_values_char,
           lt_curr        TYPE TABLE OF bapi1003_alloc_values_curr,
           lt_return      TYPE TABLE OF bapiret2,
           lv_objkey      TYPE objnum,
           lt_value_char  TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
           lv_fert_matnr  TYPE matnr,
           lv_matnr_fac   TYPE matnr,
           lv_out_matnr   TYPE matnr,
           lt_bom_details TYPE ztt_bom_item,
           lt_bom_output  TYPE ztt_bom_ip_out,
           lt_stpo        TYPE tty_stpo_level,
           lt_stpo_fac    TYPE tty_stpo_level,
           lv_req_type    TYPE zebr_t_header-req_type,
           lt_snum_asy    TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
           lt_scurr_asy   TYPE STANDARD TABLE OF bapi1003_alloc_values_curr,
           lt_schar_asy   TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
           lt_return_asy  TYPE bapiret2_t,
           lt_snum_test   TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
           lt_scurr_test  TYPE STANDARD TABLE OF bapi1003_alloc_values_curr,
           lt_schar_test  TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
           lt_return_test TYPE bapiret2_t.
    CREATE OBJECT lo_exception .
    READ  TABLE   it_key_tab  INTO DATA(ls_key_tab)   WITH KEY name  =  'Mfrpn'  .
    IF sy-subrc EQ 0.
      CALL METHOD cl_http_utility=>unescape_url
        EXPORTING
          escaped   = ls_key_tab-value
        RECEIVING
          unescaped = er_entity-mfrpn.
      READ  TABLE   it_key_tab  INTO DATA(ls_key_tab_req)   WITH KEY name  =  'ReqType'  .
      IF sy-subrc EQ 0.
        er_entity-req_type  = ls_key_tab_req-value  .

        CLEAR: lv_reqno,ls_header,
               ls_header  , lv_reqno ,
               lo_exception    , lv_matnr   , lt_num,lt_char ,
               lt_return       , lv_objkey  , lv_fert_matnr ,lt_curr .
        "
        CREATE OBJECT lo_exception .
        READ  TABLE   it_key_tab  INTO DATA(ls_key_tab_mpw)   WITH KEY name  =  'MpwWafer'  .
        IF sy-subrc EQ 0.
          er_entity-mpw_wafer  = ls_key_tab_mpw-value .
        ENDIF.
        " get requestor  and group
        SELECT SINGLE bname , accnt   FROM usr02  INTO @DATA(ls_usr)  WHERE bname = @sy-uname .
        IF ls_usr IS NOT INITIAL .
          SELECT  SINGLE employee_id ,displayname , maximorg  FROM zworkday_data  INTO @DATA(ls_workday) WHERE  employee_id = @ls_usr-accnt .
          er_entity-created_by  = ls_workday-displayname  .
          er_entity-changed_by  = ls_workday-displayname .
          er_entity-group_ebr   = ls_workday-maximorg .
        ENDIF .
        " get define product line ID and description
        SELECT de_prod_line_id ,
               prod_line_id ,
               de_prod_lin_name FROM zde_prod_line INTO TABLE  @DATA(lt_zde_prod_line)
                                                        WHERE de_prod_line_id IS NOT NULL. "#EC CI_NOFIELD
        IF sy-subrc EQ  0.
          LOOP AT  lt_zde_prod_line ASSIGNING FIELD-SYMBOL(<fs_de_prod_line>).
            TRANSLATE <fs_de_prod_line>-de_prod_lin_name TO UPPER CASE .
          ENDLOOP.
        ENDIF.

        IF er_entity-mfrpn IS INITIAL AND lo_exception IS BOUND.
          lo_exception->get_msg_container( )->add_message(
             EXPORTING
               iv_msg_type     = 'E'
               iv_msg_id      = '00'
               iv_msg_number  = '123'
               iv_msg_text    = 'No input received to backend' ).
*          IV_MSG_V1
          RAISE EXCEPTION lo_exception.
          RETURN.
        ENDIF.
        IF er_entity-mfrpn  IS NOT INITIAL .
          TRANSLATE er_entity-mfrpn TO UPPER CASE.
          SELECT SINGLE matnr ,mtart , bismt ,mfrpn FROM mara INTO  @DATA(ls_mara)  WHERE mfrpn = @er_entity-mfrpn .
          IF sy-subrc EQ 0.
            er_entity-output_part  = ls_mara-matnr .
            lv_matnr_fac           = ls_mara-matnr  .   " use this variable to  explode bom for facility
            IF ls_mara-mtart  = 'FERT'.
              lv_fert_matnr   = ls_mara-matnr .
            ELSE.

              CALL FUNCTION 'ZGET_MULTILEVEL_BOM_BOTTOM_UP'
                EXPORTING
                  im_matnr         = ls_mara-matnr
                CHANGING
                  ct_bom_details   = lt_bom_details
                  ct_bom_input_out = lt_bom_output.


              DELETE lt_bom_details  WHERE zout_mtart NE 'FERT'  .
              DELETE lt_bom_output    WHERE zout_mtart NE 'FERT'  .
              SELECT matnr , matkl FROM mara INTO TABLE @DATA(lt_mara_matgrp)
                        FOR ALL ENTRIES IN @lt_bom_output  WHERE matnr  = @lt_bom_output-zout_mat .
              IF sy-subrc EQ 0.
                DELETE  lt_mara_matgrp WHERE ( matkl  = 'F005'  OR matkl = 'F009'  OR matkl  = 'F010'  )  .
              ENDIF.
              READ TABLE lt_mara_matgrp  INTO DATA(ls_mara_matgrp) INDEX 1  .
              IF sy-subrc EQ  0.
                " get the fert material to use further  for find characteristics
                lv_fert_matnr   = ls_mara_matgrp-matnr.
              ELSE.
*                IF lo_exception IS BOUND.
*                  lo_exception->get_msg_container( )->add_message(
*                        EXPORTING
*                          iv_msg_type     = 'I'
*                          iv_msg_id      = '00'
*                          iv_msg_number  = '123'
*                          iv_msg_text    = 'Output Material' && ` ` && er_entity-mfrpn && ` ` && 'does not  have valid FERT Material' ).
*                  RAISE EXCEPTION lo_exception.
*                  RETURN.
*                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            IF  lo_exception IS BOUND.
              lo_exception->get_msg_container( )->add_message(
                       EXPORTING
                         iv_msg_type     = 'E'
                         iv_msg_id      = '00'
                         iv_msg_number  = '123'
                         iv_msg_text    = 'Output Material' && ` ` && er_entity-mfrpn && ` ` && 'does not exist' ).
              RAISE EXCEPTION lo_exception.
              RETURN.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR  lv_objkey.
        lv_objkey =   lv_fert_matnr.
        CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
          EXPORTING
            objectkey       = lv_objkey
            objecttable     = 'MARA'
            classnum        = 'BU_RESPONSIBILITY'
            classtype       = '001'
            keydate         = sy-datum
            language        = sy-langu
          TABLES
            allocvaluesnum  = lt_num
            allocvalueschar = lt_char
            allocvaluescurr = lt_curr
            return          = lt_return.

        APPEND LINES OF lt_char TO lt_value_char .
        REFRESH lt_char[].

        CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
          EXPORTING
            objectkey       = lv_objkey
            objecttable     = 'MARA'
            classnum        = 'IC_CHARACTERISTICS'
            classtype       = '001'
            keydate         = sy-datum
            language        = sy-langu
          TABLES
            allocvaluesnum  = lt_num
            allocvalueschar = lt_char
            allocvaluescurr = lt_curr
            return          = lt_return.

        APPEND LINES OF lt_char TO lt_value_char .
        REFRESH lt_char[] .

        " package code
        READ TABLE lt_value_char INTO DATA(ls_valuechar) WITH KEY charact  = 'PKGCODE'.
        IF sy-subrc EQ 0  .
          er_entity-package_code   = ls_valuechar-value_char  .
        ENDIF.

        " package type
        READ TABLE lt_value_char INTO ls_valuechar WITH KEY charact  = 'PKGTYPE'.
        IF sy-subrc EQ 0  .
          er_entity-package_type   = ls_valuechar-value_char  .
        ELSE.
          READ TABLE lt_value_char INTO ls_valuechar WITH KEY charact  = 'GENPKGTYP'.
          IF sy-subrc EQ 0  .
            er_entity-package_type   = ls_valuechar-value_char  .
          ENDIF.
        ENDIF.

        " wafer sort
*        er_entity-wafer_sort     =  'SORT'  .

        " ship to
        er_entity-ship_to        =    'MPOC'  .

        " detailed product line
        READ TABLE lt_value_char INTO ls_valuechar WITH KEY charact = 'PRODLINEDET' .
        IF sy-subrc EQ  0 .
          TRANSLATE ls_valuechar-value_char TO UPPER CASE.
          READ TABLE lt_zde_prod_line INTO DATA(ls_zde_prod_line)  WITH KEY de_prod_lin_name = ls_valuechar-value_char .
          IF sy-subrc EQ 0 .
            DATA(lv_dpl)   =   ls_zde_prod_line-de_prod_line_id .
          ENDIF.

          SELECT SINGLE prod_line_id
            FROM zde_prod_line
             INTO @DATA(lv_product_line)
             WHERE de_prod_line_id = @lv_dpl.

          " buinses unit
          SELECT SINGLE bu_id
            FROM zbucp_prod_line
            INTO @DATA(lv_business_unit)  WHERE product_line_id = @lv_product_line.

          " bu name
          SELECT SINGLE bu_unit_name
            FROM zbucp_busin_unit
            INTO @DATA(lv_bu_unit_name)
            WHERE bu_id = @lv_business_unit.
          IF sy-subrc EQ 0.
            er_entity-bu_name    = lv_bu_unit_name .
          ENDIF.
        ENDIF.
        " get bom numberf rom mast table based on  material number form screen
        CALL METHOD zcl_bom_explode_util=>bom_explode
          EXPORTING
            iv_matnr              = lv_fert_matnr
            iv_werks              = '4900'
*           ls_stpo               =
            iv_get_marterial_type = 'X'
*           iv_usage_probability  = 'X'
          CHANGING
            it_stpo               = lt_stpo.

        " get assembly characterostics
        DATA(lt_asy)   = lt_stpo   .
        DELETE lt_asy WHERE mtart NE 'ZASY'  .
        READ TABLE lt_asy  INTO DATA(ls_asy)  INDEX 1  .
        IF sy-subrc EQ 0.

          CALL METHOD zcl_zebr_form_dpc_ext=>get_mat_characteristics
            EXPORTING
              iv_matnr           = ls_asy-matnr
            IMPORTING
              et_allocvaluesnum  = lt_snum_asy
              et_allocvalueschar = lt_schar_asy
              et_allocvaluescurr = lt_scurr_asy
              et_return          = lt_return_asy.
          READ TABLE lt_schar_asy INTO DATA(ls_schar_asy)  WITH KEY charact = 'CARRIERTYPE' .
          IF sy-subrc EQ 0.
            er_entity-assy_carrier_type   = ls_schar_asy-value_char .
          ENDIF.


        ENDIF.

        " get ztst characteristics
        DATA(lt_test)   = lt_stpo   .
        DELETE lt_test WHERE mtart NE 'ZTST'  .
        READ TABLE lt_test  INTO DATA(ls_test)  INDEX 1  .
        IF sy-subrc EQ 0.
          CALL METHOD zcl_zebr_form_dpc_ext=>get_mat_characteristics
            EXPORTING
              iv_matnr           = ls_test-matnr
            IMPORTING
              et_allocvaluesnum  = lt_snum_test
              et_allocvalueschar = lt_schar_test
              et_allocvaluescurr = lt_scurr_test
              et_return          = lt_return_test.
          READ TABLE lt_schar_test INTO DATA(ls_schar_test)  WITH KEY charact = 'CARRIERTYPE' .
          IF sy-subrc EQ 0.
            er_entity-test_carrier_type   = ls_schar_test-value_char .
          ENDIF.

        ENDIF.
        " cretaed on
        er_entity-created_on    = sy-datum .
        er_entity-created_time  = sy-uzeit .
        er_entity-changed_on    = sy-datum.
        er_entity-changed_time  = sy-uzeit .

        " fast track  logic

        SELECT SINGLE * FROM zebr_config  INTO  @DATA(ls_zebr_config_req)  WHERE name  = 'REQ_TYPE' "#EC CI_ALL_FIELDS_NEEDED
                                                                            AND  type = 'DROP_DOWN'
                                                                            AND  field1  = @er_entity-req_type .
        IF sy-subrc EQ  0.
          lv_ftk  =  ls_zebr_config_req-field3 .
          SHIFT lv_ftk  LEFT DELETING LEADING  '0'  .
          SELECT SINGLE * FROM zebr_config  INTO  @DATA(ls_zebr_config_fst)  WHERE name  = 'FASTTRACK' "#EC CI_ALL_FIELDS_NEEDED
                                                                              AND  type = 'DROP_DOWN'
                                                                              AND  field2  = @lv_ftk .
          IF sy-subrc EQ  0.
            er_entity-fasttrack_no   =  ls_zebr_config_fst-field2 .
          ENDIF.
        ENDIF.

        " get route details and vendor
        " explode bom for facilities
        CALL METHOD zcl_bom_explode_util=>bom_explode
          EXPORTING
            iv_matnr              = lv_matnr_fac
            iv_werks              = '4900'
*           ls_stpo               =
            iv_get_marterial_type = 'X'
          CHANGING
            it_stpo               = lt_stpo_fac.

        " get asy route vendors
        READ TABLE  lt_stpo_fac INTO DATA(ls_stpo)  WITH KEY  mtart  =  'ZASY' .
        IF sy-subrc EQ 0.
          er_entity-asy_facility  =  'ANY' .
        ENDIF.

        " get bump route vendors
        READ TABLE  lt_stpo_fac INTO ls_stpo  WITH KEY  mtart  =  'ZBMP' .
        IF sy-subrc EQ 0.
          er_entity-bump_facility  =  'ANY' .
        ENDIF.

        SELECT a~matnr ,                           "#EC CI_NO_TRANSFORM
               a~werks ,
               a~plnty ,
               a~plnnr ,
               a~plnal ,
               b~vagrp ,
               b~ktext INTO TABLE @DATA(lt_mapl)  FROM mapl   AS a  INNER JOIN plko  AS b ON    a~plnty  =   b~plnty
                                                                                         AND   a~plnnr  =   b~plnnr
                                                                                         AND   a~plnal  =   b~plnal
                                                                                    FOR ALL ENTRIES IN @lt_stpo_fac
                                                                                     WHERE a~matnr  = @lt_stpo_fac-matnr
                                                                                      AND  a~plnty  = 'N'
                                                                                      AND   b~vagrp   =  'S'
                                                                                      AND  b~verwe   IN ('PRD','INI','NPR')
                                                                                      AND  a~loekz   EQ  ' '.
        IF sy-subrc EQ 0.
          SELECT werks , name1  FROM t001w INTO TABLE @DATA(lt_t001w) FOR ALL ENTRIES IN @lt_mapl WHERE "#EC CI_NO_TRANSFORM
                            werks  =  @lt_mapl-werks  .
          IF sy-subrc EQ 0.
            SORT lt_t001w   BY werks  .
          ENDIF.
          SELECT a~plnty  ,                        "#EC CI_NO_TRANSFORM
                 a~plnnr  ,
                 a~plnal  ,
                 a~plnkn  ,
                 a~zaehl  ,
                 b~datuv  ,
                 b~loekz  ,
                 b~vornr  ,
                 b~werks  ,
                 b~lifnr   INTO TABLE @DATA(lt_plas_plpo)  FROM plas AS a  INNER JOIN plpo AS b
                                                  ON    a~plnty   =   b~plnty
                                                 AND    a~plnnr   =   b~plnnr
                                                 AND    a~plnkn   =   b~plnkn
                                                 FOR ALL ENTRIES IN  @lt_mapl
                                               WHERE  a~plnty  =  @lt_mapl-plnty
                                                AND   a~plnnr  =  @lt_mapl-plnnr
                                                AND   a~plnal  =  @lt_mapl-plnal
                                                AND   b~werks  =  @lt_mapl-werks .
          IF sy-subrc EQ 0.
            SORT lt_plas_plpo  BY plnty plnnr plnal ASCENDING datuv  DESCENDING  .
            DELETE ADJACENT DUPLICATES FROM lt_plas_plpo   COMPARING plnty plnnr plnal vornr .
            DELETE lt_plas_plpo WHERE loekz  =  'X'  .
          ENDIF.
          CLEAR   ls_stpo .
          READ TABLE  lt_stpo_fac   INTO ls_stpo  WITH KEY mtart  =  'FERT'  .
          IF sy-subrc EQ 0.
            READ TABLE lt_mapl  INTO DATA(ls_mapl)  WITH KEY  matnr   = ls_stpo-matnr .
            IF sy-subrc EQ 0.
              er_entity-finish_facility  =  ls_mapl-plnnr  &&  ls_mapl-plnal .
              IF er_entity-mpw_wafer IS INITIAL .
                er_entity-finish_facility   =   ls_mapl-plnnr && ls_mapl-plnal .
              ELSE.
                READ TABLE lt_plas_plpo  INTO DATA(ls_plas_plpo)  WITH KEY plnty  =  ls_mapl-plnty
                                                                           plnnr  =  ls_mapl-plnnr
                                                                           plnal   =  ls_mapl-plnal
                                                                           werks  =  ls_mapl-werks .
                IF sy-subrc EQ  0   AND ls_plas_plpo-lifnr IS NOT INITIAL.
                  er_entity-finish_facility  = ls_plas_plpo-lifnr  .

                ELSE.
                  er_entity-finish_facility  = ls_mapl-werks .
                ENDIF.
              ENDIF.
            ENDIF .
          ENDIF.

          " test facility
          READ TABLE  lt_stpo_fac   INTO ls_stpo WITH KEY mtart  =  'ZTST'  .
          IF sy-subrc EQ 0.
            CLEAR ls_mapl .
            READ TABLE lt_mapl  INTO ls_mapl  WITH KEY  matnr   = ls_stpo-matnr .
            IF sy-subrc EQ 0.
              er_entity-final_tst_facility  =   ls_mapl-plnnr && ls_mapl-plnal .
              IF er_entity-mpw_wafer IS INITIAL .
                er_entity-finish_facility   =   ls_mapl-plnnr && ls_mapl-plnal .
              ELSE.
                CLEAR  ls_plas_plpo.
                READ TABLE lt_plas_plpo  INTO ls_plas_plpo  WITH KEY plnty  =  ls_mapl-plnty
                                                                           plnnr  =  ls_mapl-plnnr
                                                                           plnal   =  ls_mapl-plnal
                                                                           werks  =  ls_mapl-werks .
                IF sy-subrc EQ  0   AND ls_plas_plpo-lifnr IS NOT INITIAL.
                  er_entity-final_tst_facility  = ls_plas_plpo-lifnr  .

                ELSE.
                  er_entity-final_tst_facility  = ls_mapl-werks .
                ENDIF.
              ENDIF.
            ENDIF .

          ENDIF.

          READ TABLE  lt_stpo_fac   INTO ls_stpo  WITH KEY mtart  =  'ZPBS'  .
          IF sy-subrc EQ 0.
            CLEAR ls_mapl .
            READ TABLE lt_mapl  INTO ls_mapl  WITH KEY  matnr   = ls_stpo-matnr .
            IF sy-subrc EQ 0.
              IF er_entity-mpw_wafer IS INITIAL .
                er_entity-pbs_facility  =   ls_mapl-plnnr && ls_mapl-plnal .
              ELSE.
                CLEAR  ls_plas_plpo.
                READ TABLE lt_plas_plpo  INTO ls_plas_plpo  WITH KEY plnty  =  ls_mapl-plnty
                                                                           plnnr  =  ls_mapl-plnnr
                                                                           plnal   =  ls_mapl-plnal
                                                                           werks  =  ls_mapl-werks .
                IF sy-subrc EQ  0   AND ls_plas_plpo-lifnr IS NOT INITIAL.
                  er_entity-pbs_facility  = ls_plas_plpo-lifnr  .

                ELSE.
                  er_entity-pbs_facility  = ls_mapl-werks .
                ENDIF.
              ENDIF.
            ENDIF .

          ENDIF.

          READ TABLE  lt_stpo_fac   INTO ls_stpo  WITH KEY mtart  =  'ZSRT'  .
          IF sy-subrc EQ 0.
            CLEAR ls_mapl .
            READ TABLE lt_mapl  INTO ls_mapl  WITH KEY  matnr   = ls_stpo-matnr .
            IF sy-subrc EQ 0.
              er_entity-sort_facility  =   ls_mapl-plnnr && ls_mapl-plnal .
              IF er_entity-mpw_wafer IS INITIAL .
                er_entity-sort_facility  =   ls_mapl-plnnr && ls_mapl-plnal .
              ELSE.
                CLEAR  ls_plas_plpo.
                READ TABLE lt_plas_plpo  INTO ls_plas_plpo  WITH KEY plnty  =  ls_mapl-plnty
                                                                           plnnr  =  ls_mapl-plnnr
                                                                           plnal   =  ls_mapl-plnal
                                                                           werks  =  ls_mapl-werks .
                IF sy-subrc EQ  0   AND ls_plas_plpo-lifnr IS NOT INITIAL.
                  er_entity-sort_facility  = ls_plas_plpo-lifnr  .

                ELSE.
                  er_entity-sort_facility  = ls_mapl-werks .
                ENDIF.
              ENDIF.
            ENDIF .

          ENDIF.
        ENDIF.



        CLEAR : lv_fert_matnr ,
                lv_objkey     ,
                lt_num[]      ,
                lt_char[]     ,
                lt_curr[]     ,
                lt_return[]   ,
                lt_value_char[],
                ls_header      ,
                lv_reqno       ,
                lo_exception   ,
                lv_matnr       ,
                lt_num         ,
                lt_char        ,
                lt_curr        ,
                lt_return      ,
                lv_objkey      ,
                lt_value_char  ,
                lv_fert_matnr  ,
                lv_out_matnr   ,
                lt_bom_details ,
                lt_bom_output  ,
                lt_stpo_fac[].


*        CALL METHOD cl_http_utility=>unescape_url
*    EXPORTING
*      escaped   = ls_key_tab-value
*    RECEIVING
*      unescaped = lv_output_fert  .
*      MOVE-CORRESPONDING ls_key_tab   TO er_entity  .
      ELSE.   " req  type error
*        IF lo_exception IS  BOUND .
*          lo_exception->get_msg_container( )->add_message(
*               EXPORTING
*                 iv_msg_type     = 'E'
*                 iv_msg_id      = '00'
*                 iv_msg_number  = '123'
*                 iv_msg_text    = 'Request type  :No input received to backend' ).
**          IV_MSG_V1
*          RAISE EXCEPTION lo_exception.
*          RETURN.
*        ENDIF.
      ENDIF.
    ELSE.
*      IF lo_exception IS BOUND.
*        lo_exception->get_msg_container( )->add_message(
*             EXPORTING
*               iv_msg_type     = 'E'
*               iv_msg_id      = '00'
*               iv_msg_number  = '123'
*               iv_msg_text    = 'Part Number  :No input received to backend' ).
**          IV_MSG_V1
*        RAISE EXCEPTION lo_exception.
*        RETURN.
*      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD EBRPARTINFOSET_GET_ENTITYSET.
    data :  lv_output_fert  type matnr .
**TRY.
*CALL METHOD SUPER->EBRPARTINFOSET_GET_ENTITYSET
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
*    LOOP AT it_key_tab  INTO DATA(ls_key_tab)  .
*      IF ls_key_tab-name  = 'OutputPart'  .
*        APPEND INITIAL LINE TO et_entityset  ASSIGNING FIELD-SYMBOL(<fs_entityset>)  .
*          CALL METHOD cl_http_utility=>unescape_url
*    EXPORTING
*      escaped   = ls_key_tab-value
*    RECEIVING
*      unescaped = lv_output_fert  .
*        <fs_entityset>-output_part   = lv_output_fert   .
*      ENDIF.
*      CLEAR  :  ls_key_tab  .
*    ENDLOOP.
  ENDMETHOD.


  method EBRPBSFACSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRPBSFACSET_GET_ENTITYSET
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
        data : lt_fac  type zcl_zebr_form_mpc_ext=>tt_ebrpbsfac   .
            APPEND INITIAL LINE TO lt_fac ASSIGNING FIELD-SYMBOL(<fs_fac>)  .
    <fs_fac>-mfrpn  = ' '   .

    et_entityset  = lt_fac .

  endmethod.


  METHOD EBRPCNTYPESET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRPCNTYPESET_GET_ENTITYSET
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
    DATA: lt_taba TYPE STANDARD TABLE OF dd07v,
          lt_tabb TYPE STANDARD TABLE OF dd07v.

    CALL FUNCTION 'DD_DOMA_GET'
      EXPORTING
        domain_name   = 'ZEBR_DO_PCN_TYPE'
        langu         = sy-langu
        withtext      = 'X'
      TABLES
        dd07v_tab_a   = lt_taba
        dd07v_tab_n   = lt_tabb
      EXCEPTIONS
        illegal_value = 1
        op_failure    = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      LOOP AT lt_taba INTO DATA(ls_taba) .
        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entity>)  .
        <fs_entity>-pcn_type = ls_taba-domvalue_l .
        <fs_entity>-pcn_type_text  =  ls_taba-ddtext.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD EBRPRFSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRPRFSET_GET_ENTITYSET
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
    DATA : lrt_prf       TYPE RANGE OF  /plmb/ecrid,
           lrt_createdby TYPE RANGE OF zbucp_header-created_by.
    LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options)  .
      IF ls_filter_select_options-property  = 'PrfNo'  .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_prf)  INDEX 1.
        IF sy-subrc EQ  0 AND  ls_filter_prf-low  IS NOT INITIAL.

          TRANSLATE ls_filter_prf-low TO UPPER CASE .
          TRANSLATE ls_filter_prf-high TO UPPER CASE .
          APPEND INITIAL LINE TO lrt_prf ASSIGNING FIELD-SYMBOL(<fr_prf>).
          <fr_prf>-sign   = ls_filter_prf-sign .
          <fr_prf>-option = ls_filter_prf-option  .
          <fr_prf>-low    = ls_filter_prf-low  .
          <fr_prf>-high   = ls_filter_prf-high  .
        ENDIF.
      ENDIF.

      IF ls_filter_select_options-property  = 'CreatedBy'  .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_createdby)  INDEX 1.
        IF sy-subrc EQ  0 AND  ls_filter_createdby-low  IS NOT INITIAL.

          TRANSLATE ls_filter_createdby-low TO UPPER CASE .
          TRANSLATE ls_filter_createdby-high TO UPPER CASE .
          APPEND INITIAL LINE TO lrt_createdby ASSIGNING FIELD-SYMBOL(<fr_createdby>).
          <fr_createdby>-sign   = ls_filter_createdby-sign .
          <fr_createdby>-option = ls_filter_createdby-option  .
          <fr_createdby>-low    = ls_filter_createdby-low  .
          <fr_createdby>-high   = ls_filter_createdby-high  .
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF lrt_prf IS  NOT  INITIAL OR  lrt_createdby IS NOT INITIAL .

      SELECT  a~ecr_id  ,
              a~created_by ,
              b~die_family ,
              b~die_rev_code FROM zbucp_header AS a  INNER JOIN zbucp_item_die  AS b ON
                                         a~ecr_id  =  b~ecr_id INTO TABLE @DATA(lt_prf)
                                                          WHERE a~ecr_id  IN @lrt_prf
                                                   AND       a~created_by IN @lrt_createdby .
    ELSE.
      SELECT  a~ecr_id
          a~created_by
          b~die_family
          b~die_rev_code FROM zbucp_header AS a  INNER JOIN zbucp_item_die  AS b ON
                                     a~ecr_id  =  b~ecr_id INTO TABLE lt_prf
                                                    UP TO 100 ROWS
                                                 WHERE a~ecr_id  IN lrt_prf
                                                   AND a~created_by IN lrt_createdby .

    ENDIF.
    LOOP AT lt_prf INTO  DATA(ls_prf)  .
      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_prf>) .
      <fs_prf>-prf_no   =  ls_prf-ecr_id .
      <fs_prf>-created_by  =  ls_prf-created_by .
      <fs_prf>-die_family   =  ls_prf-die_family .
      <fs_prf>-die_rev_code   = ls_prf-die_rev_code .

*
    ENDLOOP .

  ENDMETHOD.


  method EBRRELSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRRELSET_GET_ENTITYSET
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
  endmethod.


  method EBRREQTYPESET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->EBRREQTYPESET_GET_ENTITY
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
  endmethod.


  METHOD EBRREQTYPESET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRREQTYPESET_GET_ENTITYSET
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
    DATA: it_reqtyp_so TYPE TABLE OF selopt,
          wa_reqtyp_so TYPE selopt,
          lv_cont(3).

    CLEAR: wa_reqtyp_so.

    wa_reqtyp_so-sign = 'I'.
    wa_reqtyp_so-option = 'EQ'.
    wa_reqtyp_so-low = 'REQ_TYPE'.
    APPEND wa_reqtyp_so TO  it_reqtyp_so.
    CLEAR: wa_reqtyp_so.

    wa_reqtyp_so-sign = 'I'.
    wa_reqtyp_so-option = 'EQ'.
    wa_reqtyp_so-low = 'REQ_TYPE_TXT'.
    APPEND wa_reqtyp_so TO  it_reqtyp_so.
    CLEAR: wa_reqtyp_so.

    SELECT * FROM zebr_config  INTO TABLE @DATA(lt_zebr_config)  WHERE name  IN @it_reqtyp_so.

    LOOP AT lt_zebr_config INTO DATA(ls_zebr_config).
      IF ls_zebr_config-name = 'REQ_TYPE'.
        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entityset>)  .
        <fs_entityset>-req_type      = ls_zebr_config-field1  .
        <fs_entityset>-req_desc      = ls_zebr_config-field2 .
        READ TABLE lt_zebr_config INTO ls_zebr_config WITH KEY name = 'REQ_TYPE_TXT' type = <fs_entityset>-req_type .
        IF sy-subrc EQ 0.
          LOOP AT lt_zebr_config INTO ls_zebr_config WHERE name = 'REQ_TYPE_TXT' AND type = <fs_entityset>-req_type.
            <fs_entityset>-req_det_desc  = <fs_entityset>-req_det_desc && ' ' && ls_zebr_config-field1.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD EBRREQTYPEVALUES_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->EBRREQTYPEVALUES_GET_ENTITY
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

    READ TABLE it_key_tab   INTO DATA(ls_key_tab) WITH KEY name = 'ReqNo'  .
    IF sy-subrc EQ 0.
      er_entity-req_no   = ls_key_tab-value  .
      SELECT single req_no ,req_type FROM zebr_t_header INTO @DATA(ls_header)  WHERE req_no  =  @er_entity-req_no . .

        IF sy-subrc EQ 0 .
          er_entity-req_type   =  ls_header-req_type .
          CLEAR ls_header .
        ENDIF.
      ENDIF.

  ENDMETHOD.


  METHOD EBRSENDBACKSET_CREATE_ENTITY.
**TRY.
*CALL METHOD SUPER->EBRSENDBACKSET_CREATE_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
**    io_data_provider        =
**  IMPORTING
**    er_entity               =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    DATA   lt_stat_log   TYPE STANDARD TABLE OF  zebr_t_stat_log .

    " get data from screen
    io_data_provider->read_entry_data(
   IMPORTING
   es_data = er_entity ).
    IF er_entity IS NOT INITIAL .
      SELECT * FROM zebr_t_header  INTO TABLE @DATA(lt_ebrheader)  WHERE req_no  =  @er_entity-req_no .    "#EC CI_ALL_FIELDS_NEEDED
      IF sy-subrc EQ 0 .
        LOOP AT lt_ebrheader ASSIGNING FIELD-SYMBOL(<fs_ebrheader>) .
          <fs_ebrheader>-req_status_no  = er_entity-req_status_no .
          SELECT SINGLE  field2  FROM zebr_config   INTO @DATA(lv_sendback) WHERE name    = 'REQ_STATUS'
                                              AND type    = 'DROP_DOWN'
                                              AND field1  =  @er_entity-req_status_no  .
          IF sy-subrc EQ 0 .
            er_entity-req_status          =  lv_sendback .

            <fs_ebrheader>-req_status     = lv_sendback .
            <fs_ebrheader>-npp_processor  = '' .
             CLEAR lv_sendback  .
          ENDIF.

         APPEND INITIAL LINE TO  lt_stat_log ASSIGNING FIELD-SYMBOL(<fs_stat>)  .
        <fs_stat>-req_no         = er_entity-req_no .
        <fs_stat>-req_status_no  = er_entity-req_status_no.
        <fs_stat>-req_status     = er_entity-req_status .
        <fs_stat>-created_by     = sy-uname .
        <fs_stat>-created_at     = sy-uzeit .
        <fs_stat>-created_on     = sy-datum .

        ENDLOOP .
        MODIFY zebr_t_header FROM TABLE lt_ebrheader .
        IF sy-subrc EQ 0.
          COMMIT WORK .
          er_entity-flag  =  'S'  .
          IF lt_stat_log IS NOT INITIAL .
            MODIFY zebr_t_stat_log  FROM TABLE  lt_stat_log .
          ENDIF .
        ELSE.
          er_entity-flag  = 'E'  .
        ENDIF.
      ELSE.
        er_entity-flag  = 'E'  .
      ENDIF .
    ENDIF.
  ENDMETHOD.


  METHOD EBRSHIPTOSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRSHIPTOSET_GET_ENTITYSET
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

    SELECT * FROM zebr_config  INTO TABLE @DATA(lt_zebr_config)  WHERE name  = 'SHIP_TO'              "#EC CI_ALL_FIELDS_NEEDED
                                                                  AND type = 'DROP_DOWN'  .
    IF sy-subrc EQ   0  .

      LOOP AT lt_zebr_config INTO DATA(ls_zebr_config)  .
        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entityset>)  .
        <fs_entityset>-ship_to       = ls_zebr_config-field1  .
        <fs_entityset>-ship_to_desc  = ls_zebr_config-field2 .
      ENDLOOP.

      APPEND  INITIAL LINE TO et_entityset ASSIGNING <fs_entityset>  .
      <fs_entityset>-ship_to         = 'Multiple Sites'  .
      <fs_entityset>-ship_to_desc  = 'Multiple Sites' .
    ENDIF.


  ENDMETHOD.


  METHOD EBRSTATUSLOGSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRSTATUSLOGSET_GET_ENTITYSET
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
    DATA lrt_reqno TYPE RANGE OF zebr_de_req_no  .
    LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options)  .
      IF ls_filter_select_options-property  = 'ReqNo' .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_reqno) INDEX 1 .
        IF sy-subrc EQ  0 .
          TRANSLATE  ls_filter_reqno-low  TO UPPER CASE .
          TRANSLATE  ls_filter_reqno-high  TO UPPER CASE .
          APPEND INITIAL LINE TO lrt_reqno ASSIGNING FIELD-SYMBOL(<fr_reqno>).
          <fr_reqno>-sign  =  ls_filter_reqno-sign .
          <fr_reqno>-option = ls_filter_reqno-option  .
          <fr_reqno>-low = ls_filter_reqno-low  .
          <fr_reqno>-high = ls_filter_reqno-high  .
        ENDIF.
      ENDIF.
    ENDLOOP.

    SELECT * FROM   zebr_t_stat_log  INTO TABLE @DATA(lt_stat_log) WHERE req_no   IN @lrt_reqno .
    IF lt_stat_log IS  NOT INITIAL .
      SORT lt_stat_log  by req_no  created_on created_at .

      SELECT  bname , accnt   FROM usr02  INTO  table @DATA(lt_usr) for ALL ENTRIES IN  @lt_stat_log WHERE bname = @lt_stat_log-created_by .
      IF lt_usr IS NOT INITIAL .
        sort  lt_usr by accnt .
        SELECT   employee_id ,displayname , maximorg  FROM zworkday_data INTO  table @DATA(lt_workday)
                               FOR ALL ENTRIES IN  @lt_usr  WHERE  employee_id = @lt_usr-accnt+0(10) .
      IF lt_workday IS NOT INITIAL .
        sort lt_workday  by  employee_id .
      endif.
     endif.
      LOOP AT lt_stat_log  INTO DATA(ls_stat_log)  .
        data(lv_index)   = sy-tabix  .


        APPEND INITIAL LINE TO et_entityset  ASSIGNING FIELD-SYMBOL(<fs_stat_log>)  .
        READ TABLE lt_usr  INTO data(ls_usr)  with KEY bname  =  ls_stat_log-created_by  .
        if sy-subrc  eq 0  .
          READ TABLE lt_workday INTO data(ls_workday)  with key employee_id  = ls_usr-accnt+0(10)   BINARY SEARCH .
          if sy-subrc eq 0.
            <fs_stat_log>-created_by  =  ls_workday-displayname .
           endif.

        endif.
        <fs_stat_log>-seq         =  lv_index .
        <fs_stat_log>-req_no      =  ls_stat_log-REQ_NO.
        <fs_stat_log>-created_on  =  ls_stat_log-created_on .
*        <fs_stat_log>-created_time =  ls_stat_log-created_at.

        <fs_stat_log>-new_status  =  ls_stat_log-req_status  .
        lv_index  = lv_index - 1 .
        READ TABLE lt_stat_log   into data(ls_old_status)  index  lv_index  .
        if sy-subrc eq 0.
         <fs_stat_log>-old_status   =  ls_old_status-req_status .
        ENDIF .
        clear lv_index .
      ENDLOOP.
     sort  et_entityset  by seq DESCENDING .
    ENDIF.

  ENDMETHOD.


  method EBRTESTFACSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRTESTFACSET_GET_ENTITYSET
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
    data : lt_fac  type zcl_zebr_form_mpc_ext=>tt_ebrtestfac   .
        APPEND INITIAL LINE TO lt_fac ASSIGNING FIELD-SYMBOL(<fs_fac>)  .
    <fs_fac>-mfrpn  = ' '   .

    et_entityset  = lt_fac .

  endmethod.


  method EBRVENDORSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRVENDORSET_GET_ENTITYSET
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
    data  :  lrt_lifnr type RANGE OF  lifnr ,
             lrt_name1  type RANGE OF  lfa1-name1.
    LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options)  .
      IF ls_filter_select_options-property  = 'Lifnr'  .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_lifnr)  INDEX 1.
        IF sy-subrc EQ  0 AND  ls_filter_lifnr-low  IS NOT INITIAL.

          TRANSLATE ls_filter_lifnr-low TO UPPER CASE .
          TRANSLATE ls_filter_lifnr-high TO UPPER CASE .
          APPEND INITIAL LINE TO lrt_lifnr ASSIGNING FIELD-SYMBOL(<fr_lifnr>).
          <fr_lifnr>-sign   = ls_filter_lifnr-sign .
          <fr_lifnr>-option = ls_filter_lifnr-option  .
          <fr_lifnr>-low    = ls_filter_lifnr-low  .
          <fr_lifnr>-high   = ls_filter_lifnr-high  .
        ENDIF.
      ENDIF.

            IF ls_filter_select_options-property  = 'Name1'  .
        READ TABLE ls_filter_select_options-select_options  INTO DATA(ls_filter_name1)  INDEX 1.
        IF sy-subrc EQ  0 AND  ls_filter_name1-low  IS NOT INITIAL.

          TRANSLATE ls_filter_name1-low TO UPPER CASE .
          TRANSLATE ls_filter_name1-high TO UPPER CASE .
          APPEND INITIAL LINE TO lrt_name1 ASSIGNING FIELD-SYMBOL(<fr_name1>).
          <fr_name1>-sign   = ls_filter_name1-sign .
          <fr_name1>-option = ls_filter_name1-option  .
          <fr_name1>-low    = ls_filter_name1-low  .
          <fr_name1>-high   = ls_filter_name1-high  .
        ENDIF.
      ENDIF.


    ENDLOOP.
    if lrt_lifnr   is  INITIAL   and lrt_name1  is INITIAL .
    select  lifnr , name3  from lfa1 INTO table @data(lt_lfa1)  UP TO 100 rows  where  lifnr in @lrt_lifnr
                                                                                 and name3   in @lrt_name1
                                                                                 and KTOKK   eq 'Z002'.                                                                        .
    else.
      select  lifnr name3  from lfa1 INTO table lt_lfa1  where lifnr in lrt_lifnr
                                                                  and name3   in lrt_name1
                                                                  and  KTOKK   eq 'Z002'.
    endif.

    loop at lt_lfa1  INTO DATA(ls_lfa1)   where name3 is NOT INITIAL .
      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entityset>)  .
      <fs_entityset>-lifnr   =  ls_lfa1-lifnr  .
      <fs_entityset>-name2  =   ls_lfa1-name3 .
    endloop .
  endmethod.


  method EBRWAFERNOSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->EBRWAFERNOSET_GET_ENTITYSET
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
    do 25 times .
      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entityset>) .
      <fs_entityset>-wafer_no  =   sy-index .
    enddo.
  endmethod.


  method EBRWAFERSIZESET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->EBRWAFERSIZESET_GET_ENTITY
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

    READ TABLE it_key_tab INTO data(ls_key_tab)  with key name  = 'LotNo' .
    if sy-subrc eq 0.
     er_entity-lot_no  = ls_key_tab-value .

      select single  matnr ,licha from   mch1 INTO  @data(ls_mch1)  where licha  =  @er_entity-lot_no  .
       if sy-subrc   eq 0.
         select single  matnr  , groes  from mara  INTO @data(ls_mara)  where matnr  = @ls_mch1-matnr  .
           er_entity-wafer_size  = ls_mara-groes .
       ENDIF .
    endif.
  endmethod.


  method EBRWAFERSORTSET_GET_ENTITYSET.

**TRY.
*CALL METHOD SUPER->EBRWAFERSORTSET_GET_ENTITYSET
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
   SELECT * from ZEBR_CONFIG  into table @data(lt_zebr_config)  where name  = 'WAFER_SORT'          "#EC CI_ALL_FIELDS_NEEDED
                                                                  and type = 'DROP_DOWN'  .

   loop at lt_zebr_config into data(ls_zebr_config)  .
     APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entityset>)  .
     <fs_entityset>-wafer_value  = ls_zebr_config-field1  .
   ENDLOOP.

  endmethod.


  METHOD FILL_MASTERDATA.

    DATA : lt_stpo_bom_srt   TYPE ztt_bom_com_level,
           lt_stpo_bom_fab   TYPE ztt_bom_com_level,
           lt_stpo_bom_pbs   TYPE ztt_bom_com_level,
           lt_stpo_bom_asy   TYPE ztt_bom_com_level,
           lt_stpo_bom_assy  TYPE ztt_bom_com_level,
           lt_stpo_bom_bmp   TYPE ztt_bom_com_level,
           lt_stpo_bom_tst   TYPE ztt_bom_com_level,
           lt_stpo_bom_b1_b2 TYPE ztt_bom_com_level,
           lv_matnr_fab      TYPE matnr,
           lrt_mtart_skip    TYPE RANGE OF  mtart.

    " add DC facilities from
    SELECT * FROM zebr_config  INTO TABLE @DATA(lt_zebr_config)  WHERE name  = 'DESIGN_CENTER' "#EC CI_ALL_FIELDS_NEEDED
                                                                  AND type    = 'DROP_DOWN'  .
    IF sy-subrc EQ 0.
      READ TABLE  lt_zebr_config   TRANSPORTING NO FIELDS  WITH KEY  field1  = cs_ebrlotinfo-pbs_facility .
      IF sy-subrc  EQ 0.
        APPEND INITIAL LINE TO lrt_mtart_skip  ASSIGNING FIELD-SYMBOL(<fs_mtart>)   .
        <fs_mtart>-sign    = 'I'  .
        <fs_mtart>-option  = 'EQ'  .
        <fs_mtart>-low     = 'ZPBS' .


        APPEND INITIAL LINE TO lrt_mtart_skip  ASSIGNING <fs_mtart>   .
        <fs_mtart>-sign    = 'I'  .
        <fs_mtart>-option  = 'EQ'  .
        <fs_mtart>-low     = 'ZTST' .


        APPEND INITIAL LINE TO lrt_mtart_skip  ASSIGNING <fs_mtart>   .
        <fs_mtart>-sign    = 'I'  .
        <fs_mtart>-option  = 'EQ'  .
        <fs_mtart>-low     = 'FERT' .

      ENDIF.

      READ TABLE  lt_zebr_config   TRANSPORTING NO FIELDS  WITH KEY  field1  = cs_ebrlotinfo-final_tst_facility .
      IF sy-subrc  EQ 0.


        APPEND INITIAL LINE TO lrt_mtart_skip  ASSIGNING <fs_mtart>    .
        <fs_mtart>-sign    = 'I'  .
        <fs_mtart>-option  = 'EQ'  .
        <fs_mtart>-low     = 'ZTST' .


        APPEND INITIAL LINE TO lrt_mtart_skip  ASSIGNING <fs_mtart>     .
        <fs_mtart>-sign    = 'I'  .
        <fs_mtart>-option  = 'EQ'  .
        <fs_mtart>-low     = 'FERT' .

      ENDIF.


      READ TABLE  lt_zebr_config   TRANSPORTING NO FIELDS  WITH KEY  field1  = cs_ebrlotinfo-finish_facility .
      IF sy-subrc  EQ 0.


        APPEND INITIAL LINE TO lrt_mtart_skip  ASSIGNING <fs_mtart>   .
        <fs_mtart>-sign    = 'I'  .
        <fs_mtart>-option  = 'EQ'  .
        <fs_mtart>-low     = 'FERT' .

      ENDIF.
    ENDIF.



    " if out material is zsrt  than build sort facility master data directly
    IF iv_out_mtart  EQ 'ZSRT'  .
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = cs_ebrlotinfo-sort_facility
        IMPORTING
          output = cs_ebrlotinfo-sort_facility.

      READ TABLE ct_stpo_bom  INTO DATA(ls_stpo_bom)  WITH KEY
                                                         lifnr = cs_ebrlotinfo-sort_facility
                                                         mtart   = 'ZSRT'  .
      IF sy-subrc EQ 0 .
        APPEND INITIAL LINE TO ct_master_status  ASSIGNING FIELD-SYMBOL(<fs_master_status>)  .
        <fs_master_status>-req_no          = iv_reqid .
        <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
        <fs_master_status>-mtart           = ls_stpo_bom-mtart .
        <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
        <fs_master_status>-matnr           = ls_stpo_bom-matnr .
        <fs_master_status>-bom_no          = ls_stpo_bom-bom_no .
        <fs_master_status>-bom_alt         = ls_stpo_bom-alt .
        IF cr_deep_entity-mpw_wafer  IS INITIAL  .
          <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
          <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
          <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .

        ELSE.
          READ TABLE ct_mapl INTO DATA(ls_mapl)  WITH KEY matnr  = ls_stpo_bom-matnr  .
          IF sy-subrc EQ 0 .
            READ TABLE ct_plas_plpo  INTO DATA(ls_plas_plpo)  WITH KEY plnnr = ls_mapl-plnnr
                                                                       plnal = ls_mapl-plnal
                                                                       lifnr = cs_ebrlotinfo-sort_facility.
            IF sy-subrc EQ 0.
              <fs_master_status>-route_number    = ls_mapl-plnnr .
              <fs_master_status>-route_counter   = ls_mapl-plnal .
              <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
              CLEAR : ls_plas_plpo .
            ENDIF.
            CLEAR: ls_mapl .
          ENDIF.
        ENDIF.
        CLEAR  : ls_stpo_bom .
      ELSE.
        " for  internal case  plant  will be facility for which leading zero's will not thier  so it will fail in above case
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = cs_ebrlotinfo-sort_facility
          IMPORTING
            output = cs_ebrlotinfo-sort_facility.
        READ TABLE ct_stpo_bom  INTO ls_stpo_bom WITH KEY
                                             lifnr = cs_ebrlotinfo-sort_facility
                                             mtart   = 'ZSRT'  .
        IF sy-subrc EQ 0 .
          APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
          <fs_master_status>-req_no          =  iv_reqid .
          <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
          <fs_master_status>-mtart           = ls_stpo_bom-mtart .
          <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
          <fs_master_status>-matnr           = ls_stpo_bom-matnr .
          <fs_master_status>-bom_no          = ls_stpo_bom-bom_no .
          <fs_master_status>-bom_alt         = ls_stpo_bom-alt .
          IF cr_deep_entity-mpw_wafer  IS INITIAL  .
            <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
            <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
            <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
          ELSE.
            READ TABLE ct_mapl INTO ls_mapl  WITH KEY matnr  = ls_stpo_bom-matnr
                                                      werks  = cs_ebrlotinfo-sort_facility .
            IF sy-subrc EQ 0 .
              READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                   plnal = ls_mapl-plnal
                                                                   werks = cs_ebrlotinfo-sort_facility.
              IF sy-subrc EQ 0.
                <fs_master_status>-route_number    = ls_mapl-plnnr .
                <fs_master_status>-route_counter   = ls_mapl-plnal .
                <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                CLEAR  ls_plas_plpo.
              ENDIF.
              CLEAR: ls_mapl .
            ENDIF.
          ENDIF.
          CLEAR  : ls_stpo_bom .
        ENDIF.
      ENDIF.

    ENDIF.
    "  assembly facility
    IF cs_ebrlotinfo-asy_facility  IS NOT INITIAL .

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = cs_ebrlotinfo-asy_facility
        IMPORTING
          output = cs_ebrlotinfo-asy_facility.
      IF cs_ebrlotinfo-asy_facility   NE  'ANY'  .
        DATA(lv_asy_fac) =  cs_ebrlotinfo-asy_facility  .
      ELSE.
        " if assembly vendor is  'ANY'   get foirst valid vendor for fert material
        DELETE  lt_stpo_bom_asy  WHERE mtart  NE 'ZASY'  .
        DELETE  lt_stpo_bom_asy  WHERE lifnr  IS INITIAL .
        READ TABLE  lt_stpo_bom_asy  INTO DATA(ls_stpo_bom_asy)  INDEX 1 .
        IF sy-subrc EQ 0.
          lv_asy_fac  =  ls_stpo_bom_asy-lifnr  .
        ENDIF.

      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_asy_fac
        IMPORTING
          output = lv_asy_fac.


*      READ TABLE ct_stpo_bom  INTO ls_stpo_bom  WITH KEY lifnr = lv_asy_fac
*                                                               mtart   = 'ZASY'  .
*      IF sy-subrc EQ 0 .
      LOOP  AT  ct_stpo_bom  INTO ls_stpo_bom  WHERE  lifnr = lv_asy_fac
                                                 AND  mtart   = 'ZASY'  .
        " for b1 and b2 scenario   to determine  p1  or p2 by checking  whether it contains  sort lot  material  or not .
        REFRESH : lt_stpo_bom_assy[].
        CALL METHOD zcl_bom_explode_util=>bom_explode
          EXPORTING
            iv_matnr              = ls_stpo_bom-matnr
            iv_werks              = '4900'
*           ls_stpo               =
            iv_get_marterial_type = 'X'
**             iv_active_bom         =
*           iv_bom_alt            = ls_stpo_bom-alt
            iv_active_bom_01      = 'X'
*           iv_usage_probability  = 'X'
          CHANGING
            it_stpo               = lt_stpo_bom_assy.
        READ TABLE  lt_stpo_bom_assy INTO DATA(ls_stpo_bom_assy)  WITH KEY matnr  =  iv_lot_matnr.
        IF sy-subrc EQ 0.

          APPEND INITIAL LINE TO ct_master_status  ASSIGNING  <fs_master_status>  .
          <fs_master_status>-req_no          =  iv_reqid .
          <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
          <fs_master_status>-mtart           = ls_stpo_bom-mtart .
          <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
          <fs_master_status>-matnr           = ls_stpo_bom-matnr .
          <fs_master_status>-bom_no          = ls_stpo_bom-bom_no .
          <fs_master_status>-bom_alt         = ls_stpo_bom-alt .
          LOOP AT ct_mapl INTO ls_mapl  WHERE matnr  = ls_stpo_bom-matnr
                                        AND   werks  = '4900' .
*            IF sy-subrc EQ 0 .
            READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                 plnal = ls_mapl-plnal
                                                                 lifnr = lv_asy_fac .
            IF sy-subrc EQ 0.
              <fs_master_status>-route_number    = ls_plas_plpo-plnnr .
              <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
              <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
              EXIT.
            ENDIF.
          ENDLOOP.
          " get all sort materials inside this assembly with alternate bom
          REFRESH  : lt_stpo_bom_srt  .
          CALL METHOD zcl_bom_explode_util=>bom_explode
            EXPORTING
              iv_matnr              = ls_stpo_bom-matnr
              iv_werks              = '4900'
*             ls_stpo               =
              iv_get_marterial_type = 'X'
*             iv_active_bom         =
              iv_bom_alt            = ls_stpo_bom-alt
              iv_active_bom_01      = 'X'
*             iv_usage_probability  = 'X'
            CHANGING
              it_stpo               = lt_stpo_bom_srt.

          " check  whether  its phantom die or not  .
          DATA(lt_stpo_bom_phantom) = lt_stpo_bom_srt   .
          DELETE lt_stpo_bom_phantom WHERE zdie_type IS INITIAL  .
          " if we find atleast one entry than its phantom  die and hybrid
          " assumption when multiple phamtom die are in bom if one phamtom die maitained in zphantom_die
          " all  its alternates will be available in zmm_phantom_die table
          READ TABLE  lt_stpo_bom_phantom  INTO DATA(ls_stpo_bom_phantom)  INDEX 1 .
          IF sy-subrc EQ 0.
            " if die entries are available  in zmm_phamton_die table than it is hybrid  else it is mono plus
            SELECT  * FROM   zmm_phantom_die  INTO  TABLE @DATA(lt_phantom_die) WHERE  pdietype  = @ls_stpo_bom_phantom-zdie_type "#EC CI_ALL_FIELDS_NEEDED
                                                                      AND  pdierev  = @ls_stpo_bom_phantom-zdie_revcode .
            IF sy-subrc EQ 0.
              DATA(lv_hybrid) =  abap_true .
            ENDIF.
            REFRESH  lt_phantom_die .
            CLEAR ls_stpo_bom_phantom .
          ENDIF.

          IF lv_hybrid IS NOT INITIAL .
            LOOP AT lt_stpo_bom_srt ASSIGNING FIELD-SYMBOL(<fs_stpo_bom_srt>) .  .
              CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
                EXPORTING
                  input  = <fs_stpo_bom_srt>-matnr
                IMPORTING
                  output = <fs_stpo_bom_srt>-matnr.

            ENDLOOP .
            SELECT  * FROM   zmm_phantom_die  INTO  TABLE @DATA(lt_phantom)  WHERE  pdietype  = @cs_ebrlotinfo-output_die_type
                                                                               AND  pdierev  = @cs_ebrlotinfo-output_die_rev  .
            IF sy-subrc EQ 0.
              LOOP AT lt_phantom   INTO DATA(ls_phantom) WHERE  pdietype  = cs_ebrlotinfo-output_die_type
                                                           AND  pdierev   = cs_ebrlotinfo-output_die_rev  .
                " append only valid sort's based on die
                IF ls_phantom-sort1  IS NOT INITIAL .
                  LOOP AT   lt_stpo_bom_srt INTO DATA(ls_stpo_bom_srt)  WHERE  matnr  CS ls_phantom-sort1
                                                                         AND   mtart   = 'ZSRT' .
                    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                      EXPORTING
                        input  = ls_stpo_bom_srt-matnr
                      IMPORTING
                        output = ls_stpo_bom_srt-matnr.

                    READ TABLE  ct_mch1 INTO DATA(ls_mch1)  WITH KEY  licha  =   cs_ebrlotinfo-lot_no
                                                                      matnr  =   ls_stpo_bom_srt-matnr .
                    IF sy-subrc EQ 0.
                      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                      <fs_master_status>-req_no          = iv_reqid .
                      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
                      <fs_master_status>-mtart           = 'ZSRT' .
                      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                      <fs_master_status>-matnr           = ls_stpo_bom_srt-matnr .
                      <fs_master_status>-bom_no          = ls_stpo_bom_srt-bom_no .
                      <fs_master_status>-bom_alt         = ls_stpo_bom_srt-alt .

                      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                        <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                        <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                        <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                      ELSE.
                        READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                        IF sy-subrc EQ 0 .
                          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                                     plnal = ls_mapl-plnal  .
                          IF sy-subrc EQ  0 .
                            <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                            <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                            <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                          ENDIF.
                        ENDIF.
                        CLEAR  : ls_stpo_bom_srt .
                      ENDIF.
                      CLEAR : ls_mch1 .
                    ENDIF.
                  ENDLOOP.
                ENDIF.
                " second sort
                IF ls_phantom-sort2  IS NOT INITIAL .
                  LOOP AT   lt_stpo_bom_srt INTO ls_stpo_bom_srt  WHERE  matnr  CS ls_phantom-sort2
                                                                         AND   mtart   = 'ZSRT' .
                    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                      EXPORTING
                        input  = ls_stpo_bom_srt-matnr
                      IMPORTING
                        output = ls_stpo_bom_srt-matnr.
                    READ TABLE  ct_mch1 INTO ls_mch1  WITH KEY  licha  =   cs_ebrlotinfo-lot_no
                    matnr  =   ls_stpo_bom_srt-matnr .
                    IF sy-subrc EQ 0.
                      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                      <fs_master_status>-req_no          = iv_reqid .
                      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
                      <fs_master_status>-mtart           = 'ZSRT' .
                      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                      <fs_master_status>-matnr           = ls_stpo_bom_srt-matnr .
                      <fs_master_status>-bom_no          = ls_stpo_bom_srt-bom_no .
                      <fs_master_status>-bom_alt         = ls_stpo_bom_srt-alt .
                      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                        <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                        <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                        <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                      ELSE.

                        READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                        IF sy-subrc EQ 0 .
                          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                                     plnal = ls_mapl-plnal  .
                          IF sy-subrc EQ  0 .
                            <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                            <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                            <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                          ENDIF.
                        ENDIF.
                        CLEAR  : ls_stpo_bom_srt .
                      ENDIF.
                    ENDIF.
                  ENDLOOP .
                ENDIF.

                " third sort
                IF ls_phantom-sort3  IS NOT INITIAL .
                  LOOP AT   lt_stpo_bom_srt INTO ls_stpo_bom_srt  WHERE  matnr  CS ls_phantom-sort3
                                                                         AND   mtart   = 'ZSRT' .
                    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                      EXPORTING
                        input  = ls_stpo_bom_srt-matnr
                      IMPORTING
                        output = ls_stpo_bom_srt-matnr.
                    READ TABLE  ct_mch1 INTO ls_mch1  WITH KEY  licha  =   cs_ebrlotinfo-lot_no
                                                                matnr  =   ls_stpo_bom_srt-matnr .
                    IF sy-subrc EQ 0.
                      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                      <fs_master_status>-req_no          = iv_reqid .
                      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
                      <fs_master_status>-mtart           = 'ZSRT' .
                      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                      <fs_master_status>-matnr           = ls_stpo_bom_srt-matnr .
                      <fs_master_status>-bom_no          = ls_stpo_bom_srt-bom_no .
                      <fs_master_status>-bom_alt         = ls_stpo_bom_srt-alt .
                      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                        <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                        <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                        <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                      ELSE.

                        READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                        IF sy-subrc EQ 0 .
                          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                                     plnal = ls_mapl-plnal  .
                          IF sy-subrc EQ  0 .
                            <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                            <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                            <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                          ENDIF.
                        ENDIF.
                        CLEAR  : ls_stpo_bom_srt .
                      ENDIF.
                    ENDIF.
                  ENDLOOP.
                ENDIF.

                "fourth sort
                IF ls_phantom-sort4  IS NOT INITIAL .
                  LOOP AT   lt_stpo_bom_srt INTO ls_stpo_bom_srt WHERE  matnr  CS ls_phantom-sort4
                                                                         AND   mtart   = 'ZSRT' .
                    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                      EXPORTING
                        input  = ls_stpo_bom_srt-matnr
                      IMPORTING
                        output = ls_stpo_bom_srt-matnr.
                    READ TABLE  ct_mch1 INTO ls_mch1   WITH KEY  licha  =   cs_ebrlotinfo-lot_no
                                                                 matnr  =   ls_stpo_bom_srt-matnr .
                    IF sy-subrc EQ 0.
                      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                      <fs_master_status>-req_no          = iv_reqid .
                      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
                      <fs_master_status>-mtart           = 'ZSRT' .
                      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                      <fs_master_status>-matnr           = ls_stpo_bom_srt-matnr .
                      <fs_master_status>-bom_no          = ls_stpo_bom_srt-bom_no .
                      <fs_master_status>-bom_alt         = ls_stpo_bom_srt-alt .
                      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                        <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                        <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                        <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                      ELSE.

                        READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                        IF sy-subrc EQ 0 .
                          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                                     plnal = ls_mapl-plnal  .
                          IF sy-subrc EQ  0 .
                            <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                            <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                            <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                          ENDIF.
                        ENDIF.
                        CLEAR  : ls_stpo_bom_srt .
                      ENDIF.
                    ENDIF.
                  ENDLOOP.
                ENDIF.
                "fifth sort
                IF ls_phantom-sort5  IS NOT INITIAL .
                  LOOP AT   lt_stpo_bom_srt INTO ls_stpo_bom_srt  WHERE  matnr  CS ls_phantom-sort5
                                                                         AND   mtart   = 'ZSRT' .
                    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                      EXPORTING
                        input  = ls_stpo_bom_srt-matnr
                      IMPORTING
                        output = ls_stpo_bom_srt-matnr.
                    READ TABLE  ct_mch1 INTO ls_mch1  WITH KEY  licha  =   cs_ebrlotinfo-lot_no
                                                                matnr  =   ls_stpo_bom_srt-matnr .
                    IF sy-subrc EQ 0.
                      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                      <fs_master_status>-req_no          = iv_reqid .
                      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
                      <fs_master_status>-mtart           = 'ZSRT' .
                      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                      <fs_master_status>-matnr           = ls_stpo_bom_srt-matnr .
                      <fs_master_status>-bom_no          = ls_stpo_bom_srt-bom_no .
                      <fs_master_status>-bom_alt         = ls_stpo_bom_srt-alt .
                      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                        <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                        <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                        <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                      ELSE.

                        READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                        IF sy-subrc EQ 0 .
                          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                                     plnal = ls_mapl-plnal  .
                          IF sy-subrc EQ  0 .
                            <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                            <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                            <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                          ENDIF.
                        ENDIF.
                      ENDIF.
                      CLEAR  : ls_stpo_bom_srt .
                    ENDIF.
                  ENDLOOP.

                ENDIF.
                " 6th sort

                IF ls_phantom-sort6  IS NOT INITIAL .
                  LOOP AT   lt_stpo_bom_srt INTO ls_stpo_bom_srt  WHERE  matnr  CS ls_phantom-sort6
                                                                         AND   mtart   = 'ZSRT' .
                    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                      EXPORTING
                        input  = ls_stpo_bom_srt-matnr
                      IMPORTING
                        output = ls_stpo_bom_srt-matnr.
                    READ TABLE  ct_mch1 INTO ls_mch1   WITH KEY  licha  =   cs_ebrlotinfo-lot_no
                    matnr  =   ls_stpo_bom_srt-matnr .
                    IF sy-subrc EQ 0.
                      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                      <fs_master_status>-req_no          = iv_reqid .
                      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
                      <fs_master_status>-mtart           = 'ZSRT' .
                      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                      <fs_master_status>-matnr           = ls_stpo_bom_srt-matnr .
                      <fs_master_status>-bom_no          = ls_stpo_bom_srt-bom_no .
                      <fs_master_status>-bom_alt         = ls_stpo_bom_srt-alt .
                      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                        <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                        <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                        <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                      ELSE.

                        READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                        IF sy-subrc EQ 0 .
                          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                                     plnal = ls_mapl-plnal  .
                          IF sy-subrc EQ  0 .
                            <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                            <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                            <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                          ENDIF.
                        ENDIF.
                      ENDIF.
                      CLEAR  : ls_stpo_bom_srt .
                    ENDIF.
                  ENDLOOP.
                ENDIF.


                " 7th sort

                IF ls_phantom-sort7  IS NOT INITIAL .
                  LOOP AT   lt_stpo_bom_srt INTO ls_stpo_bom_srt  WHERE  matnr  CS ls_phantom-sort7
                                                                         AND   mtart   = 'ZSRT' .
                    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                      EXPORTING
                        input  = ls_stpo_bom_srt-matnr
                      IMPORTING
                        output = ls_stpo_bom_srt-matnr.
                    READ TABLE  ct_mch1 INTO ls_mch1   WITH KEY  licha  =   cs_ebrlotinfo-lot_no
                                                                  matnr  =   ls_stpo_bom_srt-matnr .
                    IF sy-subrc EQ 0.
                      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                      <fs_master_status>-req_no          = iv_reqid .
                      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
                      <fs_master_status>-mtart           = 'ZSRT' .
                      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                      <fs_master_status>-matnr           = ls_stpo_bom_srt-matnr .
                      <fs_master_status>-bom_no          = ls_stpo_bom_srt-bom_no .
                      <fs_master_status>-bom_alt         = ls_stpo_bom_srt-alt .
                      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                        <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                        <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                        <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                      ELSE.


                        READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                        IF sy-subrc EQ 0 .
                          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                               plnal = ls_mapl-plnal  .
                          IF sy-subrc EQ  0 .
                            <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                            <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                            <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                          ENDIF.
                        ENDIF.
                      ENDIF.
                      CLEAR  : ls_stpo_bom_srt .
                    ENDIF.

                  ENDLOOP.
                ENDIF.
                "8th osrt

                IF ls_phantom-sort8  IS NOT INITIAL .
                  LOOP AT   lt_stpo_bom_srt INTO ls_stpo_bom_srt  WHERE  matnr  CS ls_phantom-sort8
                                                                         AND   mtart   = 'ZSRT' .
                    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                      EXPORTING
                        input  = ls_stpo_bom_srt-matnr
                      IMPORTING
                        output = ls_stpo_bom_srt-matnr.
                    READ TABLE  ct_mch1 INTO ls_mch1   WITH KEY  licha  =   cs_ebrlotinfo-lot_no
                                                                  matnr  =   ls_stpo_bom_srt-matnr .
                    IF sy-subrc EQ 0.
                      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                      <fs_master_status>-req_no          = iv_reqid .
                      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
                      <fs_master_status>-mtart           = 'ZSRT' .
                      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                      <fs_master_status>-matnr           = ls_stpo_bom_srt-matnr .
                      <fs_master_status>-bom_no          = ls_stpo_bom_srt-bom_no .
                      <fs_master_status>-bom_alt         = ls_stpo_bom_srt-alt .
                      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                        <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                        <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                        <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                      ELSE.

                        READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                        IF sy-subrc EQ 0 .
                          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                                     plnal = ls_mapl-plnal  .
                          IF sy-subrc EQ  0 .
                            <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                            <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                            <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                          ENDIF.
                        ENDIF.
                      ENDIF.
                      CLEAR  : ls_stpo_bom_srt .

                    ENDIF.
                  ENDLOOP.
                ENDIF.

                CLEAR  ls_phantom .
              ENDLOOP.
            ENDIF.


          ELSE.
            " delete  sort inside sort  inside   this is multi level sort case
            " need to consider only highest material  sort
            LOOP AT lt_stpo_bom_srt   INTO  DATA(ls_stpo_mult_srt)  .
              DATA(lv_tabix)  = sy-tabix .
              IF ls_stpo_mult_srt-higher_mtart  =  'ZSRT'    AND ls_stpo_mult_srt-mtart  = 'ZSRT'  .
                DELETE   lt_stpo_bom_srt   INDEX  lv_tabix.
              ENDIF.
              CLEAR  : ls_stpo_mult_srt ,
                       lv_tabix.
            ENDLOOP.
            LOOP AT lt_stpo_bom_srt   INTO ls_stpo_bom_srt  WHERE  mtart  = 'ZSRT'  .
              " append only valid sort's based on die
              CLEAR  lv_matnr_fab .

              lv_matnr_fab  =  cs_ebrlotinfo-output_die_type &&  '-'  && cs_ebrlotinfo-output_die_rev  .
              CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                EXPORTING
                  input        = lv_matnr_fab
                IMPORTING
                  output       = lv_matnr_fab
                EXCEPTIONS
                  length_error = 1
                  OTHERS       = 2.
              IF sy-subrc <> 0.
* Implement suitable error handling here
              ENDIF.

              " explode sort material
              CALL METHOD zcl_bom_explode_util=>bom_explode
                EXPORTING
                  iv_matnr              = ls_stpo_bom_srt-matnr
                  iv_werks              = '4900'
*                 ls_stpo               =
                  iv_get_marterial_type = 'X'
*                 iv_active_bom         =
                  iv_bom_alt            = ls_stpo_bom_srt-alt
                  iv_active_bom_01      = 'X'
*                 iv_usage_probability  = 'X'
                CHANGING
                  it_stpo               = lt_stpo_bom_fab.

              " sort which contains valid fab
              READ TABLE lt_stpo_bom_fab  TRANSPORTING NO FIELDS WITH KEY matnr  = lv_matnr_fab .
              IF sy-subrc EQ 0 .


                APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                <fs_master_status>-req_no   = iv_reqid .
                <fs_master_status>-lot_no   = cs_ebrlotinfo-lot_no  .
                <fs_master_status>-mtart           = 'ZSRT' .
                <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                <fs_master_status>-matnr    = ls_stpo_bom_srt-matnr .
                <fs_master_status>-bom_no   = ls_stpo_bom_srt-bom_no .
                <fs_master_status>-bom_alt  = ls_stpo_bom_srt-alt .

                IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                  <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                  <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                  <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                ELSE.
                  READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                  IF sy-subrc EQ 0 .
                    READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                               plnal = ls_mapl-plnal  .
                    IF sy-subrc EQ  0 .
                      <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                      <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                      <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
              CLEAR  : ls_stpo_bom_srt .
              REFRESH  lt_stpo_bom_fab[] .
            ENDLOOP.
          ENDIF.
          CLEAR  ls_stpo_bom .
          EXIT.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF cs_ebrlotinfo-bump_facility  IS NOT INITIAL .
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = cs_ebrlotinfo-bump_facility
        IMPORTING
          output = cs_ebrlotinfo-bump_facility.
      IF cs_ebrlotinfo-bump_facility   NE  'ANY'  .
        DATA(lv_bump_fac) =  cs_ebrlotinfo-bump_facility  .
      ELSE.
        " if bump vendor is  'ANY'   get foirst valid vendor for fert material
        DELETE  lt_stpo_bom_bmp  WHERE mtart  NE 'ZBMP'  .
        DELETE  lt_stpo_bom_bmp  WHERE lifnr  IS INITIAL .
        READ TABLE  lt_stpo_bom_bmp  INTO DATA(ls_stpo_bom_bmp)  INDEX 1 .
        IF sy-subrc EQ 0.
          lv_bump_fac  =  ls_stpo_bom_bmp-lifnr  .
        ENDIF.

      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_bump_fac
        IMPORTING
          output = lv_bump_fac.
*
*        READ TABLE lt_stpo_bom  INTO ls_stpo_bom  WITH KEY lifnr = lv_bump_fac
*                                                                 mtart   = 'ZBMP'  .
      LOOP  AT  ct_stpo_bom  INTO ls_stpo_bom  WHERE  lifnr = lv_bump_fac
                                                 AND  mtart   = 'ZBMP'  .
        " for b1 and b2 scenario   to determine  p1  or p2 by checking  whether it contains  sort lot  material  or not .
        REFRESH : lt_stpo_bom_b1_b2[].
        CALL METHOD zcl_bom_explode_util=>bom_explode
          EXPORTING
            iv_matnr              = ls_stpo_bom-matnr
            iv_werks              = '4900'
*           ls_stpo               =
            iv_get_marterial_type = 'X'
**             iv_active_bom         =
*           iv_bom_alt            = ls_stpo_bom-alt
            iv_active_bom_01      = 'X'
*           iv_usage_probability  = 'X'
          CHANGING
            it_stpo               = lt_stpo_bom_b1_b2.
        READ TABLE  lt_stpo_bom_b1_b2 INTO DATA(ls_stpo_bom_b1_b2)  WITH KEY matnr  =  iv_lot_matnr.
        IF sy-subrc EQ 0.


          APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
          <fs_master_status>-req_no   = iv_reqid .
          <fs_master_status>-lot_no   = cs_ebrlotinfo-lot_no  .
          <fs_master_status>-mtart           = ls_stpo_bom-mtart .
          <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
          <fs_master_status>-matnr    = ls_stpo_bom-matnr .
          <fs_master_status>-bom_no   = ls_stpo_bom-bom_no .
          <fs_master_status>-bom_alt  = ls_stpo_bom-alt .
          LOOP AT ct_mapl INTO   ls_mapl  WHERE  matnr  = ls_stpo_bom-matnr
                                           AND   werks  = '4900'  .
*            IF sy-subrc EQ 0 .
            READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                       plnal = ls_mapl-plnal
                                                                       lifnr = lv_bump_fac.
            IF sy-subrc EQ 0.
              <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
              <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
              <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
              EXIT .
            ENDIF.
*            ENDIF.
            CLEAR: ls_mapl .
          ENDLOOP.
          " get all sort materials inside this bump with alternate bom
          REFRESH : lt_stpo_bom_srt ,
                    lt_stpo_bom_phantom,
                    lt_stpo_bom_fab .
          CLEAR   : lv_matnr_fab .

          CALL METHOD zcl_bom_explode_util=>bom_explode
            EXPORTING
              iv_matnr              = ls_stpo_bom-matnr
              iv_werks              = '4900'
*             ls_stpo               =
              iv_get_marterial_type = 'X'
*             iv_active_bom         =
              iv_bom_alt            = ls_stpo_bom-alt
              iv_active_bom_01      = 'X'
*             iv_usage_probability  = 'X'
            CHANGING
              it_stpo               = lt_stpo_bom_srt.

          " check  whether  its phantom die or not  .
          lt_stpo_bom_phantom = lt_stpo_bom_srt   .
          DELETE lt_stpo_bom_phantom WHERE zdie_type IS INITIAL  .
          " if we find atleast one entry than its phantom  die and hybrid
          READ TABLE  lt_stpo_bom_phantom  TRANSPORTING NO FIELDS  INDEX 1 .
          IF sy-subrc EQ 0.
            lv_hybrid =  abap_true .
          ENDIF.

          IF lv_hybrid IS NOT INITIAL .
            SELECT  * FROM   zmm_phantom_die  INTO  TABLE lt_phantom  WHERE  pdietype  = cs_ebrlotinfo-output_die_type
                                                                       AND  pdierev  = cs_ebrlotinfo-output_die_rev  .
            IF sy-subrc EQ 0.
              LOOP AT   lt_stpo_bom_srt ASSIGNING FIELD-SYMBOL(<fs_bom_srt>)  WHERE mtart  = 'ZSRT'  .
                CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
                  EXPORTING
                    input  = <fs_bom_srt>-matnr
                  IMPORTING
                    output = <fs_bom_srt>-matnr.

              ENDLOOP .

              LOOP AT lt_phantom   INTO ls_phantom WHERE  pdietype  = cs_ebrlotinfo-output_die_type
                                                     AND  pdierev  = cs_ebrlotinfo-output_die_rev  .
                " append only valid sort's based on die
                IF ls_phantom-sort1  IS NOT INITIAL .
                  LOOP AT   lt_stpo_bom_srt INTO ls_stpo_bom_srt  WHERE  matnr  CS ls_phantom-sort1
                                                                         AND   mtart   = 'ZSRT' .
                    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                      EXPORTING
                        input  = ls_stpo_bom_srt-matnr
                      IMPORTING
                        output = ls_stpo_bom_srt-matnr.
                    READ TABLE  ct_mch1 INTO ls_mch1   WITH KEY  licha  =   cs_ebrlotinfo-lot_no
                    matnr  =   ls_stpo_bom_srt-matnr .
                    IF sy-subrc EQ 0.
                      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                      <fs_master_status>-req_no          = iv_reqid .
                      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
                      <fs_master_status>-mtart           = 'ZSRT' .
                      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .

                      <fs_master_status>-matnr           = ls_stpo_bom_srt-matnr .
                      <fs_master_status>-bom_no          = ls_stpo_bom_srt-bom_no .
                      <fs_master_status>-bom_alt         = ls_stpo_bom_srt-alt .
                      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                        <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                        <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                        <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                      ELSE.


                        READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                        IF sy-subrc EQ 0 .
                          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                                     plnal = ls_mapl-plnal  .
                          IF sy-subrc EQ  0 .
                            <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                            <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                            <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                          ENDIF.
                        ENDIF.
                      ENDIF.
                      CLEAR  : ls_stpo_bom_srt ,
                               ls_mapl ,
                               ls_plas_plpo.
                    ENDIF.
                  ENDLOOP.
                ENDIF.
                " second sort
                IF ls_phantom-sort2  IS NOT INITIAL .
                  LOOP AT   lt_stpo_bom_srt INTO ls_stpo_bom_srt  WHERE  matnr  CS ls_phantom-sort2
                                                                         AND   mtart   = 'ZSRT' .
                    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                      EXPORTING
                        input  = ls_stpo_bom_srt-matnr
                      IMPORTING
                        output = ls_stpo_bom_srt-matnr.
                    READ TABLE  ct_mch1 INTO ls_mch1  WITH KEY  licha  =   cs_ebrlotinfo-lot_no
                    matnr  =   ls_stpo_bom_srt-matnr .
                    IF sy-subrc EQ 0.
                      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                      <fs_master_status>-req_no          = iv_reqid .
                      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
                      <fs_master_status>-mtart           = 'ZSRT' .
                      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                      <fs_master_status>-matnr           = ls_stpo_bom_srt-matnr .
                      <fs_master_status>-bom_no          = ls_stpo_bom_srt-bom_no .
                      <fs_master_status>-bom_alt         = ls_stpo_bom_srt-alt .
                      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                        <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                        <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                        <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                      ELSE.

                        READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                        IF sy-subrc EQ 0 .
                          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                                     plnal = ls_mapl-plnal  .
                          IF sy-subrc EQ  0 .
                            <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                            <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                            <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                          ENDIF.
                        ENDIF.
                      ENDIF.
                      CLEAR  : ls_stpo_bom_srt ,
                               ls_mapl ,
                               ls_plas_plpo.
                    ENDIF.
                  ENDLOOP.
                ENDIF.

                " third sort
                IF ls_phantom-sort3  IS NOT INITIAL .
                  LOOP AT   lt_stpo_bom_srt INTO ls_stpo_bom_srt WHERE  matnr  CS ls_phantom-sort3
                                                                         AND   mtart   = 'ZSRT' .
                    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                      EXPORTING
                        input  = ls_stpo_bom_srt-matnr
                      IMPORTING
                        output = ls_stpo_bom_srt-matnr.
                    READ TABLE  ct_mch1 INTO ls_mch1   WITH KEY  licha  =   cs_ebrlotinfo-lot_no
                    matnr  =   ls_stpo_bom_srt-matnr .
                    IF sy-subrc EQ 0.
                      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                      <fs_master_status>-req_no          = iv_reqid .
                      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
                      <fs_master_status>-mtart           = 'ZSRT' .
                      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                      <fs_master_status>-matnr           = ls_stpo_bom_srt-matnr .
                      <fs_master_status>-bom_no          = ls_stpo_bom_srt-bom_no .
                      <fs_master_status>-bom_alt         = ls_stpo_bom_srt-alt .
                      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                        <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                        <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                        <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                      ELSE.

                        READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                        IF sy-subrc EQ 0 .
                          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                                     plnal = ls_mapl-plnal  .
                          IF sy-subrc EQ  0 .
                            <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                            <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                            <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                          ENDIF.
                        ENDIF.
                      ENDIF .
                      CLEAR  : ls_stpo_bom_srt ,
                               ls_mapl ,
                               ls_plas_plpo.
                    ENDIF.
                  ENDLOOP.
                ENDIF.

                "fourth sort
                IF ls_phantom-sort4  IS NOT INITIAL .
                  LOOP AT   lt_stpo_bom_srt INTO ls_stpo_bom_srt  WHERE  matnr  CS ls_phantom-sort4
                                                                         AND   mtart   = 'ZSRT' .
                    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                      EXPORTING
                        input  = ls_stpo_bom_srt-matnr
                      IMPORTING
                        output = ls_stpo_bom_srt-matnr.
                    READ TABLE  ct_mch1 INTO ls_mch1   WITH KEY  licha  =   cs_ebrlotinfo-lot_no
                    matnr  =   ls_stpo_bom_srt-matnr .
                    IF sy-subrc EQ 0.
                      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                      <fs_master_status>-req_no          = iv_reqid .
                      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
                      <fs_master_status>-mtart           = 'ZSRT' .
                      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                      <fs_master_status>-matnr           = ls_stpo_bom_srt-matnr .
                      <fs_master_status>-bom_no          = ls_stpo_bom_srt-bom_no .
                      <fs_master_status>-bom_alt         = ls_stpo_bom_srt-alt .
                      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                        <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                        <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                        <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                      ELSE.

                        READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                        IF sy-subrc EQ 0 .
                          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                                     plnal = ls_mapl-plnal  .
                          IF sy-subrc EQ  0 .
                            <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                            <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                            <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                          ENDIF.
                        ENDIF.
                      ENDIF.
                      CLEAR  : ls_stpo_bom_srt ,
                               ls_mapl ,
                               ls_plas_plpo.
                    ENDIF.
                  ENDLOOP.
                ENDIF.
                "fifth sort
                IF ls_phantom-sort5  IS NOT INITIAL .
                  LOOP AT   lt_stpo_bom_srt INTO ls_stpo_bom_srt  WHERE  matnr  CS ls_phantom-sort5
                                                                         AND   mtart   = 'ZSRT' .
                    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                      EXPORTING
                        input  = ls_stpo_bom_srt-matnr
                      IMPORTING
                        output = ls_stpo_bom_srt-matnr.
                    READ TABLE  ct_mch1 INTO ls_mch1   WITH KEY  licha  =   cs_ebrlotinfo-lot_no
                    matnr  =   ls_stpo_bom_srt-matnr .
                    IF sy-subrc EQ 0.
                      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                      <fs_master_status>-req_no          = iv_reqid .
                      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
                      <fs_master_status>-mtart           = 'ZSRT' .
                      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                      <fs_master_status>-matnr           = ls_stpo_bom_srt-matnr .
                      <fs_master_status>-bom_no          = ls_stpo_bom_srt-bom_no .
                      <fs_master_status>-bom_alt         = ls_stpo_bom_srt-alt .
                      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                        <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                        <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                        <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                      ELSE.

                        READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                        IF sy-subrc EQ 0 .
                          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                                     plnal = ls_mapl-plnal  .
                          IF sy-subrc EQ  0 .
                            <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                            <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                            <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                          ENDIF.
                        ENDIF.
                      ENDIF.
                      CLEAR  : ls_stpo_bom_srt ,
                               ls_mapl ,
                               ls_plas_plpo.
                    ENDIF.
                  ENDLOOP .
                ENDIF.
                " 6th sort

                IF ls_phantom-sort6  IS NOT INITIAL .
                  LOOP AT   lt_stpo_bom_srt INTO ls_stpo_bom_srt  WHERE  matnr  CS ls_phantom-sort6
                                                                         AND   mtart   = 'ZSRT' .
                    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                      EXPORTING
                        input  = ls_stpo_bom_srt-matnr
                      IMPORTING
                        output = ls_stpo_bom_srt-matnr.
                    READ TABLE  ct_mch1 INTO ls_mch1   WITH KEY  licha  =   cs_ebrlotinfo-lot_no
                    matnr  =   ls_stpo_bom_srt-matnr .
                    IF sy-subrc EQ 0.
                      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                      <fs_master_status>-req_no          = iv_reqid .
                      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
                      <fs_master_status>-mtart           = 'ZSRT' .
                      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                      <fs_master_status>-matnr           = ls_stpo_bom_srt-matnr .
                      <fs_master_status>-bom_no          = ls_stpo_bom_srt-bom_no .
                      <fs_master_status>-bom_alt         = ls_stpo_bom_srt-alt .
                      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                        <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                        <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                        <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                      ELSE.

                        READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                        IF sy-subrc EQ 0 .
                          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                                     plnal = ls_mapl-plnal  .
                          IF sy-subrc EQ  0 .
                            <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                            <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                            <fs_master_status>-old_route        = ls_mapl-plnnr_alt .
                          ENDIF.
                        ENDIF.
                      ENDIF.
                      CLEAR  : ls_stpo_bom_srt ,
                               ls_mapl ,
                               ls_plas_plpo.
                    ENDIF.
                  ENDLOOP.
                ENDIF.

                " 7th sort

                IF ls_phantom-sort7  IS NOT INITIAL .
                  LOOP AT   lt_stpo_bom_srt INTO ls_stpo_bom_srt  WHERE  matnr  CS ls_phantom-sort7
                                                                           AND   mtart   = 'ZSRT' .
                    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                      EXPORTING
                        input  = ls_stpo_bom_srt-matnr
                      IMPORTING
                        output = ls_stpo_bom_srt-matnr.
                    READ TABLE  ct_mch1 INTO ls_mch1  WITH KEY  licha  =   cs_ebrlotinfo-lot_no
                    matnr  =   ls_stpo_bom_srt-matnr .
                    IF sy-subrc EQ 0.
                      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                      <fs_master_status>-req_no          = iv_reqid .
                      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
                      <fs_master_status>-mtart           = 'ZSRT' .
                      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                      <fs_master_status>-matnr           = ls_stpo_bom_srt-matnr .
                      <fs_master_status>-bom_no          = ls_stpo_bom_srt-bom_no .
                      <fs_master_status>-bom_alt         = ls_stpo_bom_srt-alt .
                      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                        <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                        <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                        <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                      ELSE.

                        READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                        IF sy-subrc EQ 0 .
                          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                               plnal = ls_mapl-plnal  .
                          IF sy-subrc EQ  0 .
                            <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                            <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                            <fs_master_status>-old_route        = ls_mapl-plnnr_alt .
                          ENDIF.
                        ENDIF.
                      ENDIF.
                      CLEAR  : ls_stpo_bom_srt ,
                               ls_mapl ,
                               ls_plas_plpo.
                    ENDIF.
                  ENDLOOP.
                ENDIF.

                "8th osrt

                IF ls_phantom-sort8  IS NOT INITIAL .
                  LOOP AT   lt_stpo_bom_srt INTO ls_stpo_bom_srt  WHERE  matnr  CS ls_phantom-sort8
                                                                         AND   mtart   = 'ZSRT' .
                    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                      EXPORTING
                        input  = ls_stpo_bom_srt-matnr
                      IMPORTING
                        output = ls_stpo_bom_srt-matnr.
                    READ TABLE  ct_mch1 INTO ls_mch1   WITH KEY  licha  =   cs_ebrlotinfo-lot_no
                    matnr  =   ls_stpo_bom_srt-matnr .
                    IF sy-subrc EQ 0.
                      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                      <fs_master_status>-req_no          = iv_reqid .
                      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
                      <fs_master_status>-mtart           = 'ZSRT' .
                      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                      <fs_master_status>-matnr           = ls_stpo_bom_srt-matnr .
                      <fs_master_status>-bom_no          = ls_stpo_bom_srt-bom_no .
                      <fs_master_status>-bom_alt         = ls_stpo_bom_srt-alt .
                      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                        <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                        <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                        <fs_master_status>-old_route       = cs_ebrlotinfo-sort_plnnr_alt .
                      ELSE.
                        READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                        IF sy-subrc EQ 0 .
                          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                               plnal = ls_mapl-plnal  .
                          IF sy-subrc EQ  0 .
                            <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                            <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                            <fs_master_status>-old_route        = ls_mapl-plnnr_alt .
                          ENDIF.
                        ENDIF.
                      ENDIF.
                      CLEAR  : ls_stpo_bom_srt ,
                               ls_mapl ,
                               ls_plas_plpo.
                    ENDIF.
                  ENDLOOP.
                ENDIF.

                CLEAR  ls_phantom .
              ENDLOOP.
            ENDIF.

          ELSE.
            " delete  sort inside sort  inside   this is multi level sort case
            " need to consider only highest material  sort
            LOOP AT lt_stpo_bom_srt   INTO  ls_stpo_mult_srt  .
              lv_tabix  = sy-tabix .
              IF ls_stpo_mult_srt-higher_mtart  =  'ZSRT'    AND ls_stpo_mult_srt-mtart  = 'ZSRT'  .
                DELETE   lt_stpo_bom_srt   INDEX  lv_tabix.
              ENDIF.
              CLEAR  : ls_stpo_mult_srt ,
                       lv_tabix.
            ENDLOOP.
            LOOP AT lt_stpo_bom_srt   INTO ls_stpo_bom_srt  WHERE  mtart  = 'ZSRT'  .
              " append only valid sort's based on die
              CLEAR  lv_matnr_fab .
              lv_matnr_fab  =  cs_ebrlotinfo-output_die_type &&  '-'  && cs_ebrlotinfo-output_die_rev  .
              CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                EXPORTING
                  input        = lv_matnr_fab
                IMPORTING
                  output       = lv_matnr_fab
                EXCEPTIONS
                  length_error = 1
                  OTHERS       = 2.
              IF sy-subrc <> 0.
* Implement suitable error handling here
              ENDIF.


              " explode sort material
              CALL METHOD zcl_bom_explode_util=>bom_explode
                EXPORTING
                  iv_matnr              = ls_stpo_bom_srt-matnr
                  iv_werks              = '4900'
*                 ls_stpo               =
                  iv_get_marterial_type = 'X'
*                 iv_active_bom         =
                  iv_bom_alt            = ls_stpo_bom_srt-alt
                  iv_active_bom_01      = 'X'
*                 iv_usage_probability  = 'X'
                CHANGING
                  it_stpo               = lt_stpo_bom_fab.

              " sort which contains valid fab
              READ TABLE lt_stpo_bom_fab  TRANSPORTING NO FIELDS WITH KEY matnr  = lv_matnr_fab .
              IF sy-subrc EQ 0 .


                APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
                <fs_master_status>-req_no   = iv_reqid .
                <fs_master_status>-lot_no   = cs_ebrlotinfo-lot_no  .
                <fs_master_status>-mtart           = 'ZSRT' .
                <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
                <fs_master_status>-matnr    = ls_stpo_bom_srt-matnr .
                <fs_master_status>-bom_no   = ls_stpo_bom_srt-bom_no .
                <fs_master_status>-bom_alt  = ls_stpo_bom_srt-alt .
                IF cr_deep_entity-mpw_wafer  IS INITIAL  .
                  <fs_master_status>-route_number    = cs_ebrlotinfo-sort_plnnr .
                  <fs_master_status>-route_counter   = cs_ebrlotinfo-sort_plnal .
                ELSE.

                  READ TABLE ct_mapl INTO   ls_mapl  WITH KEY matnr  = ls_stpo_bom_srt-matnr  .
                  IF sy-subrc EQ 0 .
                    READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                               plnal = ls_mapl-plnal  .
                    IF sy-subrc EQ  0 .
                      <fs_master_status>-route_number     = ls_plas_plpo-plnnr .
                      <fs_master_status>-route_counter    = ls_plas_plpo-plnal .
                      <fs_master_status>-old_route        = ls_mapl-plnnr_alt .
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
              CLEAR  : ls_stpo_bom_srt ,
                       ls_mapl ,
                       ls_plas_plpo.
              REFRESH  lt_stpo_bom_fab[] .
            ENDLOOP.
          ENDIF.
          CLEAR  : ls_stpo_bom .
          EXIT .
        ELSE .
          CONTINUE .
        ENDIF.
      ENDLOOP.

    ENDIF.
    " pbs facility

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = cs_ebrlotinfo-pbs_facility
      IMPORTING
        output = cs_ebrlotinfo-pbs_facility.
    " since both vendor and plant are stored in same field   to remove leading zero's for  plant and carry leading zero's  for vendor
    SELECT  SINGLE lifnr FROM lfa1 INTO @DATA(lv_pbs_lifnr)   WHERE  lifnr =  @cs_ebrlotinfo-pbs_facility.
    IF sy-subrc  NE   0.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = cs_ebrlotinfo-pbs_facility
        IMPORTING
          output = cs_ebrlotinfo-pbs_facility.

    ENDIF.

    LOOP  AT  ct_stpo_bom  INTO ls_stpo_bom  WHERE   lifnr = cs_ebrlotinfo-pbs_facility
                                                     AND  mtart   = 'ZPBS'  .
      " for p1 and p2 scenario   to determine  p1  or p2 by checking  whether it contains  sort lot  material  or not .
      CALL METHOD zcl_bom_explode_util=>bom_explode
        EXPORTING
          iv_matnr              = ls_stpo_bom-matnr
          iv_werks              = '4900'
*         ls_stpo               =
          iv_get_marterial_type = 'X'
**             iv_active_bom         =
*         iv_bom_alt            = ls_stpo_bom-alt
          iv_active_bom_01      = 'X'
*         iv_usage_probability  = 'X'
        CHANGING
          it_stpo               = lt_stpo_bom_pbs.
      READ TABLE  lt_stpo_bom_pbs  INTO DATA(ls_stpo_bom_pbs)  WITH KEY matnr  =  iv_lot_matnr .
      IF sy-subrc EQ 0.
        APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
        <fs_master_status>-req_no          =  iv_reqid .
        <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
        <fs_master_status>-mtart           = ls_stpo_bom-mtart .
        <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
        <fs_master_status>-matnr           = ls_stpo_bom-matnr .
        <fs_master_status>-bom_no          = ls_stpo_bom-bom_no .
        <fs_master_status>-bom_alt         = ls_stpo_bom-alt .
        IF cr_deep_entity-mpw_wafer  IS INITIAL  .
          <fs_master_status>-route_number    = cs_ebrlotinfo-pbs_plnnr .
          <fs_master_status>-route_counter   = cs_ebrlotinfo-pbs_plnal .
          <fs_master_status>-old_route       = cs_ebrlotinfo-pbs_plnnr_alt .
        ELSE.
          IF lv_pbs_lifnr IS  NOT INITIAL .                                 " search for vendor
            READ TABLE ct_mapl INTO ls_mapl  WITH KEY matnr  = ls_stpo_bom-matnr .
            IF sy-subrc EQ 0 .
              READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                   plnal = ls_mapl-plnal
                                                                   lifnr = cs_ebrlotinfo-pbs_facility.
              IF sy-subrc EQ 0.
                <fs_master_status>-route_number    = ls_mapl-plnnr .
                <fs_master_status>-route_counter   = ls_mapl-plnal .
                <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                CLEAR: ls_mapl ,
                       ls_plas_plpo.
              ENDIF.
            ENDIF.

          ELSE.
            READ TABLE ct_mapl INTO ls_mapl  WITH KEY matnr  = ls_stpo_bom-matnr
                                                      werks  = cs_ebrlotinfo-pbs_facility  .
            IF sy-subrc EQ 0 .
              READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                   plnal = ls_mapl-plnal
                                                                   werks = cs_ebrlotinfo-pbs_facility.
              IF sy-subrc EQ 0.
                <fs_master_status>-route_number    = ls_mapl-plnnr .
                <fs_master_status>-route_counter   = ls_mapl-plnal .
                <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                CLEAR: ls_mapl ,
                       ls_plas_plpo.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        EXIT.
      ELSE.
        CONTINUE .
      ENDIF.
    ENDLOOP .

    " final test facility
*      IF cs_ebrlotinfo-final_tst_facility  IS NOT INITIAL .
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = cs_ebrlotinfo-final_tst_facility
      IMPORTING
        output = cs_ebrlotinfo-final_tst_facility.

    " since both vendor and plant are stored in same field   to remove leading zero's for  plant and carry leading zero's  for vendor
    SELECT  SINGLE lifnr FROM lfa1 INTO @DATA(lv_tst_lifnr)   WHERE  lifnr =  @cs_ebrlotinfo-final_tst_facility.
    IF sy-subrc  NE   0.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = cs_ebrlotinfo-final_tst_facility
        IMPORTING
          output = cs_ebrlotinfo-final_tst_facility.
    ENDIF.

*    READ TABLE ct_stpo_bom  INTO ls_stpo_bom  WITH KEY
*                                                       lifnr = cs_ebrlotinfo-final_tst_facility
*                                                       mtart   = 'ZTST'  .
*    IF sy-subrc EQ 0 .
    LOOP  AT  ct_stpo_bom  INTO ls_stpo_bom  WHERE   lifnr = cs_ebrlotinfo-final_tst_facility
                                                   AND  mtart   = 'ZTST'  .
      " for p1 and p2 scenario   to determine  p1  or p2 by checking  whether it contains  sort lot  material  or not .
      CALL METHOD zcl_bom_explode_util=>bom_explode
        EXPORTING
          iv_matnr              = ls_stpo_bom-matnr
          iv_werks              = '4900'
*         ls_stpo               =
          iv_get_marterial_type = 'X'
**        iv_active_bom         =
*         iv_bom_alt            = ls_stpo_bom-alt
          iv_active_bom_01      = 'X'
        CHANGING
          it_stpo               = lt_stpo_bom_tst.
      READ TABLE  lt_stpo_bom_tst  INTO DATA(ls_stpo_bom_tst)  WITH KEY matnr  =  iv_lot_matnr .
      IF sy-subrc EQ 0.

        APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
        <fs_master_status>-req_no          =  iv_reqid .
        <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
        <fs_master_status>-mtart           = ls_stpo_bom-mtart .
        <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
        <fs_master_status>-matnr           = ls_stpo_bom-matnr .
        <fs_master_status>-bom_no          = ls_stpo_bom-bom_no .
        <fs_master_status>-bom_alt         = ls_stpo_bom-alt .
        IF cr_deep_entity-mpw_wafer IS INITIAL .
          <fs_master_status>-route_number    = cs_ebrlotinfo-final_tst_plnnr .
          <fs_master_status>-route_counter   = cs_ebrlotinfo-final_tst_plnal .
          <fs_master_status>-old_route       = cs_ebrlotinfo-final_tst_plnnr_alt .
        ELSE.
          IF lv_tst_lifnr IS  NOT INITIAL .                                 " search for vendor
            READ TABLE ct_mapl INTO ls_mapl  WITH KEY matnr  = ls_stpo_bom-matnr .
            IF sy-subrc EQ 0 .
              READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                   plnal = ls_mapl-plnal
                                                                   lifnr = cs_ebrlotinfo-final_tst_facility.
              IF sy-subrc EQ 0.
                <fs_master_status>-route_number    = ls_mapl-plnnr .
                <fs_master_status>-route_counter   = ls_mapl-plnal .
                <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                CLEAR: ls_mapl ,
                       ls_plas_plpo.
              ENDIF.
            ENDIF.

          ELSE.
            READ TABLE ct_mapl INTO ls_mapl  WITH KEY matnr  = ls_stpo_bom-matnr
                                                      werks  = cs_ebrlotinfo-final_tst_facility  .
            IF sy-subrc EQ 0 .
              READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                   plnal = ls_mapl-plnal
                                                                   werks = cs_ebrlotinfo-final_tst_facility .
              IF sy-subrc EQ 0.
                <fs_master_status>-route_number    = ls_mapl-plnnr .
                <fs_master_status>-route_counter   = ls_mapl-plnal .
                <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
                CLEAR: ls_mapl ,
                       ls_plas_plpo.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        EXIT.
      ELSE.
        CONTINUE .
      ENDIF.
    ENDLOOP.

*      ENDIF.
*      " finish facility
*      IF cs_ebrlotinfo-finish_facility  IS NOT INITIAL .
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = cs_ebrlotinfo-finish_facility
      IMPORTING
        output = cs_ebrlotinfo-finish_facility.

    READ TABLE ct_stpo_bom  INTO ls_stpo_bom  WITH KEY lifnr = cs_ebrlotinfo-finish_facility
                                                       mtart   = 'FERT'  .
    IF sy-subrc EQ 0 .
      APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
      <fs_master_status>-req_no          =  iv_reqid .
      <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
      <fs_master_status>-matnr           = ls_stpo_bom-matnr .
      <fs_master_status>-mtart           = ls_stpo_bom-mtart .
      <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
      <fs_master_status>-bom_no          = ls_stpo_bom-bom_no .
      <fs_master_status>-bom_alt         = ls_stpo_bom-alt .
      IF cr_deep_entity-mpw_wafer  IS INITIAL  .
        <fs_master_status>-route_number    = cs_ebrlotinfo-finish_plnnr .
        <fs_master_status>-route_counter   = cs_ebrlotinfo-finish_plnal .
        <fs_master_status>-old_route       = cs_ebrlotinfo-finish_plnnr_alt .
      ELSE .
        READ TABLE ct_mapl INTO ls_mapl  WITH KEY matnr  = ls_stpo_bom-matnr  .
        IF sy-subrc EQ 0 .
          READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                               plnal = ls_mapl-plnal
                                                               lifnr = cs_ebrlotinfo-finish_facility  .
          IF sy-subrc EQ 0.
            <fs_master_status>-route_number    = ls_mapl-plnnr .
            <fs_master_status>-route_counter   = ls_mapl-plnal .
            <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
            CLEAR: ls_mapl ,
                   ls_plas_plpo.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = cs_ebrlotinfo-finish_facility
        IMPORTING
          output = cs_ebrlotinfo-finish_facility.
      READ TABLE ct_stpo_bom  INTO ls_stpo_bom  WITH KEY
                                           lifnr = cs_ebrlotinfo-finish_facility
                                           mtart   = 'FERT'  .
      IF sy-subrc EQ 0 .

        APPEND INITIAL LINE TO ct_master_status  ASSIGNING <fs_master_status>  .
        <fs_master_status>-req_no          =  iv_reqid .
        <fs_master_status>-lot_no          = cs_ebrlotinfo-lot_no  .
        <fs_master_status>-matnr           = ls_stpo_bom-matnr .
        <fs_master_status>-mtart           = ls_stpo_bom-mtart .
        <fs_master_status>-output_matnr    = cs_ebrlotinfo-output_part .
        <fs_master_status>-bom_no          = ls_stpo_bom-bom_no .
        <fs_master_status>-bom_alt         = ls_stpo_bom-alt .
        IF cr_deep_entity-mpw_wafer  IS INITIAL  .
          <fs_master_status>-route_number    = cs_ebrlotinfo-finish_plnnr .
          <fs_master_status>-route_counter   = cs_ebrlotinfo-finish_plnal .
          <fs_master_status>-old_route       = cs_ebrlotinfo-finish_plnnr_alt .
        ELSE.
          READ TABLE ct_mapl INTO ls_mapl  WITH KEY matnr  = ls_stpo_bom-matnr
                                                    werks = cs_ebrlotinfo-finish_facility  .  .
          IF sy-subrc EQ 0 .
            READ TABLE ct_plas_plpo  INTO ls_plas_plpo  WITH KEY plnnr = ls_mapl-plnnr
                                                                       plnal = ls_mapl-plnal
                                                                       werks = cs_ebrlotinfo-finish_facility.
            IF sy-subrc EQ 0.
              <fs_master_status>-route_number    = ls_mapl-plnnr .
              <fs_master_status>-route_counter   = ls_mapl-plnal .
              <fs_master_status>-old_route       = ls_mapl-plnnr_alt .
              CLEAR: ls_mapl ,
                     ls_plas_plpo.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    " delete status in master data  that need to be not considered  in based on fert alternate
    IF crt_mtart IS NOT INITIAL .
      DELETE   ct_master_status WHERE mtart  NOT IN   crt_mtart .
    ENDIF .

    " delte status in master data   when Design center is slected in PBS and final test and fert .
    IF lrt_mtart_skip  IS NOT INITIAL .
      DELETE   ct_master_status WHERE mtart  IN    lrt_mtart_skip .
    ENDIF.
    CLEAR : lv_pbs_lifnr .
  ENDMETHOD.


  METHOD FIRST_BUILD.
   " determine   first buy or not
    READ TABLE CS_EBRHEADER-ebrheadlotnav   INTO DATA(ls_headlotnav)  INDEX 1  .             "  to get assembly vendor  .
    IF sy-subrc EQ 0.
      DATA(lv_assy_fac)  =   ls_headlotnav-asy_facility.
      SHIFT lv_assy_fac LEFT DELETING LEADING '0'  .

      SELECT SINGLE  matnr , bismt ,
                     mtart , meins ,
                     mfrpn
        FROM mara
        INTO @DATA(ls_mara_bismt)
       WHERE mfrpn = @CS_EBRHEADER-mfrpn .

      SELECT SINGLE  expnr  FROM zcrossref_sto INTO  @DATA(lv_expnr) WHERE sursys = '5'
                                                                      AND flag    =  '3'     " flag value for assembly
                                                                      AND parvw   = 'LF'
                                                                      AND inpnr = @lv_assy_fac.
      IF sy-subrc EQ  0  .
        " old way of storing data in zscm_load_plan expnr
        SELECT SINGLE planning_id  INTO @DATA(lv_planid)  FROM zscm_load_plan   WHERE output_material  =  @ls_mara_bismt-bismt "#EC CI_NOFIELD
                                                                                 AND  asy_facility  =  @lv_expnr .
        IF sy-subrc EQ 0.
          CS_EBRHEADER-first_build  = 'No'.
        ELSE.
          SELECT SINGLE planning_id  INTO lv_planid  FROM zscm_load_plan   WHERE output_material  =  CS_EBRHEADER-mfrpn "#EC CI_NOFIELD
                                                                                   AND  asy_facility  =  lv_assy_fac.
          IF sy-subrc EQ 0.
            CS_EBRHEADER-first_build  = 'No'.
          ELSE.
            CS_EBRHEADER-first_build  = 'Yes'.
          ENDIF.
        ENDIF.
      ELSE.
        CS_EBRHEADER-first_build  = 'No'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD GET_BATCH_CHAR.

    DATA : lv_objkey      TYPE bapi1003_key-object,
           lv_classnum    TYPE  bapi1003_key-classnum,
           lv_classtype   TYPE  bapi1003_key-classtype,
           lt_allocations TYPE TABLE OF api_kssk.

           clear    :      lv_objkey  , lt_allocations[].
    lv_objkey = iv_matnr.
    CALL FUNCTION 'CLAP_DDB_GET_CLASSIFICATION'
      EXPORTING
        object                 = lv_objkey
        obtab                  = 'MARA'
*       STANDARD_TYPE_ONLY     = ' '
*       OBJECT_EQ_CLASS        = ' '
*       SPRAS                  = SY-LANGU
*       ONLY_VARKLART          = ' '
*       POBTAB                 = ' '
*       DATE_OF_CHANGE         =
*       CHANGE_SERVICE_NUMBER  =
        classtype              = '023'
*       READ_ONLY              = ' '
*       CALLED_FROM_API        = ' '
*       SORT_POSNR             = ' '
*           IMPORTING
*       ERROR_STATU            =
      TABLES
        allocations            = lt_allocations
      EXCEPTIONS
        no_allocation          = 1
        foreign_lock           = 2
        system_failure         = 3
        set_aennr              = 4
        change_nr_not_exist    = 5
        date_in_past           = 6
        error_class            = 7
        error_date_restriction = 8
        error_status           = 9
        error_class_status     = 10
        OTHERS                 = 11.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CLEAR: lv_classnum.
    READ TABLE lt_allocations INTO DATA(ls_allocations) INDEX 1.
    IF sy-subrc EQ 0.
      lv_classnum = ls_allocations-class.
    ENDIF.
    CLEAR lv_objkey.
    lv_objkey = iv_matnr && iv_charg.
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_objkey
        objecttable     = 'MCH1'
        classnum        = lv_classnum
        classtype       = '023'
*       KEYDATE         = SY-DATUM
*       UNVALUATED_CHARS       = ' '
*       LANGUAGE        = SY-LANGU
*       OBJECTKEY_LONG  =
* IMPORTING
*       STATUS          =
*       STANDARDCLASS   =
      TABLES
        allocvaluesnum  = et_allocvaluesnum
        allocvalueschar = et_allocvalueschar
        allocvaluescurr = et_allocvaluescurr
        return          = et_return.
  ENDMETHOD.


  METHOD GET_FERT_CHARACTERISTICS.
    DATA : lv_fert_matnr  TYPE matnr,
           lt_num         TYPE TABLE OF bapi1003_alloc_values_num,
           lt_char        TYPE TABLE OF bapi1003_alloc_values_char,
           lt_curr        TYPE TABLE OF bapi1003_alloc_values_curr,
           lt_return      TYPE TABLE OF bapiret2,
           lt_bom_details TYPE ztt_bom_item,
           lt_bom_output  TYPE ztt_bom_ip_out.
    SELECT SINGLE matnr ,mtart , bismt ,mfrpn FROM mara INTO  @DATA(ls_mara)  WHERE matnr  =  @iv_matnr.
    IF sy-subrc EQ 0.
      IF ls_mara-mtart  = 'FERT'.
        lv_fert_matnr   = ls_mara-matnr .
      ELSE.

        CALL FUNCTION 'ZGET_MULTILEVEL_BOM_BOTTOM_UP'
          EXPORTING
            im_matnr         = ls_mara-matnr
          CHANGING
            ct_bom_details   = lt_bom_details
            ct_bom_input_out = lt_bom_output.


        DELETE lt_bom_details  WHERE zout_mtart NE 'FERT'  .
        DELETE lt_bom_output    WHERE zout_mtart NE 'FERT'  .
        SELECT matnr , matkl FROM mara INTO TABLE @DATA(lt_mara_matgrp)
                  FOR ALL ENTRIES IN @lt_bom_output  WHERE matnr  = @lt_bom_output-zout_mat .
        IF sy-subrc EQ 0.
          DELETE  lt_mara_matgrp WHERE ( matkl  = 'F005'  OR matkl = 'F009'  OR matkl  = 'F010'  )  .
        ENDIF.
        READ TABLE lt_mara_matgrp  INTO DATA(ls_mara_matgrp) INDEX 1  .
        IF sy-subrc EQ  0.
          " get the fert material to use further  for find characteristics
          lv_fert_matnr   = ls_mara_matgrp-matnr.
        ENDIF.
      ENDIF.
    ENDIF.
      CALL METHOD zcl_zebr_form_dpc_ext=>get_mat_characteristics
          EXPORTING
            iv_matnr           =  lv_fert_matnr
          IMPORTING
            et_allocvaluesnum  = et_allocvaluesnum
            et_allocvalueschar = et_allocvalueschar
            et_allocvaluescurr = et_allocvaluescurr
            et_return          = et_return.
      READ TABLE et_allocvalueschar   INTO data(ls_char)   with key   charact  =  'PRODUCT_TYPE'  .
      if sy-subrc  eq 0 .
        ev_product_type  =  ls_char-value_neutral   .
        clear ls_char .
      ENDIF .


  ENDMETHOD.


  METHOD GET_MAT_CHARACTERISTICS.
    DATA: lv_obj    TYPE bapi1003_key-object,
          lt_class  TYPE STANDARD TABLE OF api_kssk,
          lw_class  TYPE api_kssk,
          lt_snum   TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
          lt_scurr  TYPE STANDARD TABLE OF bapi1003_alloc_values_curr,
          lt_char   TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
          lw_char   TYPE bapi1003_alloc_values_char,
          lt_return TYPE TABLE OF bapiret2.

    lv_obj = iv_matnr.
    CALL FUNCTION 'CLAP_DDB_GET_CLASSIFICATION'
      EXPORTING
        object                 = lv_obj
        obtab                  = 'MARA'
      TABLES
        allocations            = lt_class
      EXCEPTIONS
        no_allocation          = 1
        foreign_lock           = 2
        system_failure         = 3
        set_aennr              = 4
        change_nr_not_exist    = 5
        date_in_past           = 6
        error_class            = 7
        error_date_restriction = 8
        error_status           = 9
        OTHERS                 = 10.

    IF sy-subrc EQ 0.
      LOOP AT lt_class INTO lw_class WHERE klart = '001'.

        CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
          EXPORTING
            objectkey       = lv_obj
            objecttable     = 'MARA'
            classnum        = lw_class-class
            classtype       = lw_class-klart
            keydate         = sy-datum
          TABLES
            allocvaluesnum  = lt_snum
            allocvalueschar = lt_char
            allocvaluescurr = lt_scurr
            return          = lt_return.
        IF NOT lt_char[] IS INITIAL.
          APPEND LINES OF lt_char   TO et_allocvalueschar.
          APPEND LINES OF lt_snum   TO  et_allocvaluesnum .
          APPEND LINES OF lt_scurr  TO et_allocvaluescurr .
          APPEND LINES OF lt_return TO et_return .
        ENDIF.
        REFRESH  : lt_char[],
                   lt_snum[],
                    lt_scurr[] ,
                    lt_return[] .
        CLEAR  : lw_class.
      ENDLOOP.
    ENDIF.

    CLEAR lv_obj.
  ENDMETHOD.


  METHOD GET_PART_DETAILS.


    TYPES: ts_stpo_level  TYPE  zbom_com_level,
           tty_stpo_level TYPE ztt_bom_com_level.

    DATA : lv_output_fert TYPE matnr,
           lv_ftk         TYPE zebr_config-field2,
           ls_header      TYPE zebr_t_header,
           lv_reqno       TYPE  zebr_t_header-req_no,
           lo_exception   TYPE REF TO /iwbep/cx_mgw_tech_exception,
           lv_matnr       TYPE matnr,
           lt_num         TYPE TABLE OF bapi1003_alloc_values_num,
           lt_char        TYPE TABLE OF bapi1003_alloc_values_char,
           lt_curr        TYPE TABLE OF bapi1003_alloc_values_curr,
           lt_return      TYPE TABLE OF bapiret2,
           lv_objkey      TYPE objnum,
           lt_value_char  TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
           lv_fert_matnr  TYPE matnr,
           lv_matnr_fac   TYPE matnr,
           lv_out_matnr   TYPE matnr,
           lt_bom_details TYPE ztt_bom_item,
           lt_bom_output  TYPE ztt_bom_ip_out,
           lt_stpo        TYPE tty_stpo_level,
           lt_stpo_fac    TYPE tty_stpo_level,
           lv_req_type    TYPE zebr_t_header-req_type,
           lt_snum_asy    TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
           lt_scurr_asy   TYPE STANDARD TABLE OF bapi1003_alloc_values_curr,
           lt_schar_asy   TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
           lt_return_asy  TYPE bapiret2_t,
           lt_snum_test   TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
           lt_scurr_test  TYPE STANDARD TABLE OF bapi1003_alloc_values_curr,
           lt_schar_test  TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
           lt_return_test TYPE bapiret2_t.

    " get define product line ID and description
    SELECT de_prod_line_id ,
           prod_line_id ,
           de_prod_lin_name FROM zde_prod_line INTO TABLE  @DATA(lt_zde_prod_line)
                                                    WHERE de_prod_line_id IS NOT NULL. "#EC CI_NOFIELD
    IF sy-subrc EQ  0.
      LOOP AT  lt_zde_prod_line ASSIGNING FIELD-SYMBOL(<fs_de_prod_line>).
        TRANSLATE <fs_de_prod_line>-de_prod_lin_name TO UPPER CASE .
      ENDLOOP.
    ENDIF.


    SELECT SINGLE matnr ,mtart , bismt ,mfrpn FROM mara INTO  @DATA(ls_mara)  WHERE mfrpn = @iv_mfrpn .
    IF sy-subrc EQ 0.

      lv_matnr_fac          = ls_mara-matnr  .   " use this variable to  explode bom for facility
      IF ls_mara-mtart  = 'FERT'.
        lv_fert_matnr   = ls_mara-matnr .
      ELSE.

        CALL FUNCTION 'ZGET_MULTILEVEL_BOM_BOTTOM_UP'
          EXPORTING
            im_matnr         = ls_mara-matnr
          CHANGING
            ct_bom_details   = lt_bom_details
            ct_bom_input_out = lt_bom_output.


        DELETE lt_bom_details  WHERE zout_mtart NE 'FERT'  .
        DELETE lt_bom_output    WHERE zout_mtart NE 'FERT'  .
        SELECT matnr , matkl FROM mara INTO TABLE @DATA(lt_mara_matgrp)
                  FOR ALL ENTRIES IN @lt_bom_output  WHERE matnr  = @lt_bom_output-zout_mat .
        IF sy-subrc EQ 0.
          DELETE  lt_mara_matgrp WHERE ( matkl  = 'F005'  OR matkl = 'F009'  OR matkl  = 'F010'  )  .
        ENDIF.
        READ TABLE lt_mara_matgrp  INTO DATA(ls_mara_matgrp) INDEX 1  .
        IF sy-subrc EQ  0.
          " get the fert material to use further  for find characteristics
          lv_fert_matnr   = ls_mara_matgrp-matnr.
        ENDIF.
      ENDIF.

    ENDIF.
    CLEAR  lv_objkey.
    lv_objkey =   lv_fert_matnr.
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_objkey
        objecttable     = 'MARA'
        classnum        = 'BU_RESPONSIBILITY'
        classtype       = '001'
        keydate         = sy-datum
        language        = sy-langu
      TABLES
        allocvaluesnum  = lt_num
        allocvalueschar = lt_char
        allocvaluescurr = lt_curr
        return          = lt_return.

    APPEND LINES OF lt_char TO lt_value_char .
    REFRESH lt_char[].

    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_objkey
        objecttable     = 'MARA'
        classnum        = 'IC_CHARACTERISTICS'
        classtype       = '001'
        keydate         = sy-datum
        language        = sy-langu
      TABLES
        allocvaluesnum  = lt_num
        allocvalueschar = lt_char
        allocvaluescurr = lt_curr
        return          = lt_return.

    APPEND LINES OF lt_char TO lt_value_char .
    REFRESH lt_char[] .

    " package code
    READ TABLE lt_value_char INTO DATA(ls_valuechar) WITH KEY charact  = 'PKGCODE'.
    IF sy-subrc EQ 0  .
      cs_ebrheader-package_code   = ls_valuechar-value_char  .
    ENDIF.

    " package type
    READ TABLE lt_value_char INTO ls_valuechar WITH KEY charact  = 'PKGTYPE'.
    IF sy-subrc EQ 0  .
      cs_ebrheader-package_type   = ls_valuechar-value_char  .
    ELSE.
      READ TABLE lt_value_char INTO ls_valuechar WITH KEY charact  = 'GENPKGTYP'.
      IF sy-subrc EQ 0  .
        cs_ebrheader-package_type   = ls_valuechar-value_char  .
      ENDIF.
    ENDIF.



    " detailed product line
    READ TABLE lt_value_char INTO ls_valuechar WITH KEY charact = 'PRODLINEDET' .
    IF sy-subrc EQ  0 .
      TRANSLATE ls_valuechar-value_char TO UPPER CASE.
      READ TABLE lt_zde_prod_line INTO DATA(ls_zde_prod_line)  WITH KEY de_prod_lin_name = ls_valuechar-value_char .
      IF sy-subrc EQ 0 .
        DATA(lv_dpl)   =   ls_zde_prod_line-de_prod_line_id .
      ENDIF.

      SELECT SINGLE prod_line_id
        FROM zde_prod_line
         INTO @DATA(lv_product_line)
         WHERE de_prod_line_id = @lv_dpl.

      " buinses unit
      SELECT SINGLE bu_id
        FROM zbucp_prod_line
        INTO @DATA(lv_business_unit)  WHERE product_line_id = @lv_product_line.

      " bu name
      SELECT SINGLE bu_unit_name
        FROM zbucp_busin_unit
        INTO @DATA(lv_bu_unit_name)
        WHERE bu_id = @lv_business_unit.
      IF sy-subrc EQ 0.
        cs_ebrheader-bu_name    = lv_bu_unit_name .
      ENDIF.
    ENDIF.
    " get bom numberf rom mast table based on  material number form screen
    CALL METHOD zcl_bom_explode_util=>bom_explode
      EXPORTING
        iv_matnr              = lv_fert_matnr
        iv_werks              = '4900'
*       ls_stpo               =
        iv_get_marterial_type = 'X'
*        iv_usage_probability  = 'X'
      CHANGING
        it_stpo               = lt_stpo.

    " get assembly characterostics
    DATA(lt_asy)   = lt_stpo   .
    DELETE lt_asy WHERE mtart NE 'ZASY'  .
    READ TABLE lt_asy  INTO DATA(ls_asy)  INDEX 1  .
    IF sy-subrc EQ 0.

      CALL METHOD zcl_zebr_form_dpc_ext=>get_mat_characteristics
        EXPORTING
          iv_matnr           = ls_asy-matnr
        IMPORTING
          et_allocvaluesnum  = lt_snum_asy
          et_allocvalueschar = lt_schar_asy
          et_allocvaluescurr = lt_scurr_asy
          et_return          = lt_return_asy.
      READ TABLE lt_schar_asy INTO DATA(ls_schar_asy)  WITH KEY charact = 'CARRIERTYPE' .
      IF sy-subrc EQ 0.
        cs_ebrheader-assy_carrier_type   = ls_schar_asy-value_char .
      ENDIF.


    ENDIF.

    " get ztst characteristics
    DATA(lt_test)   = lt_stpo   .
    DELETE lt_test WHERE mtart NE 'ZTST'  .
    READ TABLE lt_test  INTO DATA(ls_test)  INDEX 1  .
    IF sy-subrc EQ 0.
      CALL METHOD zcl_zebr_form_dpc_ext=>get_mat_characteristics
        EXPORTING
          iv_matnr           = ls_test-matnr
        IMPORTING
          et_allocvaluesnum  = lt_snum_test
          et_allocvalueschar = lt_schar_test
          et_allocvaluescurr = lt_scurr_test
          et_return          = lt_return_test.
      READ TABLE lt_schar_test INTO DATA(ls_schar_test)  WITH KEY charact = 'CARRIERTYPE' .
      IF sy-subrc EQ 0.
        cs_ebrheader-test_carrier_type   = ls_schar_test-value_char .
      ENDIF.

    ENDIF.

    CLEAR : lv_fert_matnr ,
    lv_objkey     ,
    lt_num[]      ,
    lt_char[]     ,
    lt_curr[]     ,
    lt_return[]   ,
    lt_value_char[],
    ls_header      ,
    lv_reqno       ,
    lo_exception   ,
    lv_matnr       ,
    lt_num         ,
    lt_char        ,
    lt_curr        ,
    lt_return      ,
    lv_objkey      ,
    lt_value_char  ,
    lv_fert_matnr  ,
    lv_out_matnr   ,
    lt_bom_details ,
    lt_bom_output  ,
    lt_stpo_fac[].
  ENDMETHOD.


  METHOD READ_TEXT.
    DATA lt_lines       TYPE TABLE OF tline.
    CLEAR text.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = id
        language                = 'E'
        name                    = name
        object                  = object
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
* IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_lines INTO DATA(ls_line).
        IF NOT text IS INITIAL.
          IF ls_line-tdformat =  '*' .
            text = text && cl_abap_char_utilities=>newline && ls_line-tdline.
          ELSE.
            text = text && ls_line-tdline.
          ENDIF.
        ELSE.
          text = ls_line-tdline.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CONDENSE text .
  ENDMETHOD.


  METHOD SAVE_TEXT.

    DATA : ls_thead TYPE thead,
           lt_lines TYPE TABLE OF tline,
           ls_lines TYPE  tline,
           lv_len   TYPE i VALUE '72'.

    DATA : BEGIN OF ls_tline,
             tdline TYPE  tdline,
           END OF ls_tline,
           lt_tline LIKE TABLE OF ls_tline.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = id
        language                = 'E'
        name                    = name
        object                  = object
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
* IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF NOT sy-subrc IS INITIAL.
      DATA(lv_insert) = abap_true.
    ENDIF.
*    " add ing extra space  if   a word is ending position  at multiple of 72  like 72 , 144 .
*    DATA(lv_text)      =  text .
*    DATA(lv_text_len)  =  strlen( lv_text )  .
*
*    WHILE lv_len < lv_text_len .
*      IF lv_text+lv_len(1)  EQ  ' ' .
*        DATA(lv_text_space)   =  lv_text(lv_len)  &&  space  && lv_text+lv_len .
*        lv_text      =  lv_text_space .
*      ENDIF.
*      lv_len =  lv_len + 72 .
*      lv_text_len  =  strlen( lv_text )  .
*      clear  lv_text_space.
*    ENDWHILE.
    REFRESH lt_lines.
*    CALL FUNCTION 'VB_CP_CONVERT_STRING_2_ITF'
*      EXPORTING
*        i_string = text
*      TABLES
*        et_table = lt_lines.

    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = text
      TABLES
        ftext_tab = lt_tline.

    LOOP AT lt_tline INTO ls_tline.
      ls_lines-tdline = ls_tline-tdline.
      APPEND ls_lines TO lt_lines.
      CLEAR ls_lines.
    ENDLOOP.


    IF lt_lines IS INITIAL.
      ev_failed = abap_true.
    ENDIF.
    ls_thead-mandt = sy-mandt.
    ls_thead-tdid  = id.
    ls_thead-tdname = name.
    ls_thead-tdobject = object.
    ls_thead-tdspras  = 'E'.
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = sy-mandt
        header          = ls_thead
        insert          = lv_insert
        savemode_direct = abap_false
*       OWNER_SPECIFIED = ' '
*       LOCAL_CAT       = ' '
* IMPORTING
*       FUNCTION        =
*       NEWHEADER       =
      TABLES
        lines           = lt_lines
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'COMMIT_TEXT'
        EXPORTING
          savemode_direct = abap_true.
    ELSE.
      ev_failed = abap_true.
    ENDIF.

  ENDMETHOD.


  method SAVE_TEXT_CHAT.

    DATA : ls_thead       TYPE thead,
         lt_lines       TYPE TABLE OF tline,
         lt_lines_read      TYPE TABLE OF tline  ,
         ls_lines_read      type tline  .

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = id
        language                = 'E'
        name                    = name
        object                  = object
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
* IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = lt_lines_read
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF NOT sy-subrc IS INITIAL.
      DATA(lv_insert) = abap_true.
    ENDIF.
    REFRESH lt_lines.
    CALL FUNCTION 'VB_CP_CONVERT_STRING_2_ITF'
      EXPORTING
        i_string = text
      TABLES
        et_table = lt_lines.

    IF lt_lines IS INITIAL.
      ev_failed = abap_true.
    ENDIF.
    if lt_lines_read is NOT INITIAL  and lt_lines is NOT INITIAL .
      APPEND INITIAL LINE TO lt_lines ASSIGNING FIELD-SYMBOL(<fs_lines>) .
      <fs_lines>-tdformat  = '*' .
      <fs_lines>-tdline    = '________________________________________' .
    ENDIF.
    append lines of lt_lines_read   to lt_lines  .
    ls_thead-mandt = sy-mandt.
    ls_thead-tdid  = id.
    ls_thead-tdname = name.
    ls_thead-tdobject = object.
    ls_thead-tdspras  = 'E'.
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = sy-mandt
        header          = ls_thead
        insert          = lv_insert
        savemode_direct = abap_false
*       OWNER_SPECIFIED = ' '
*       LOCAL_CAT       = ' '
* IMPORTING
*       FUNCTION        =
*       NEWHEADER       =
      TABLES
        lines           = lt_lines
 EXCEPTIONS
       ID              = 1
       LANGUAGE        = 2
       NAME            = 3
       OBJECT          = 4
       OTHERS          = 5
      .
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'COMMIT_TEXT'
        EXPORTING
          savemode_direct = abap_true.
    ELSE.
      ev_failed = abap_true.
    ENDIF.

     IF sy-subrc IS INITIAL.
      LOOP AT lt_lines INTO DATA(ls_line).
        IF NOT chat_text IS INITIAL.
         chat_text = chat_text && cl_abap_char_utilities=>newline && ls_line-tdline.
        ELSE.
          Chat_text = ls_line-tdline.
        ENDIF.
      ENDLOOP.
    ENDIF.
  endmethod.


  METHOD SUBMIT_EBR.
    DATA  : lt_load_plan    TYPE STANDARD TABLE OF   zscm_load_plan,
            ls_load_plan    TYPE   zscm_load_plan,
            l_plan_id       TYPE ebelp,
            lv_input_mtart  TYPE  mtart,
            lv_name         TYPE thead-tdname,
            lv_planning_id  TYPE zscm_load_plan-planning_id,
            lo_exception    TYPE REF TO /iwbep/cx_mgw_tech_exception,
            lv_inpnr        TYPE zcrossref_sto-inpnr,
            lt_ebrlot       TYPE STANDARD TABLE OF zebr_t_lotinfo,
            lt_stpo_bom_max TYPE ztt_bom_com_level.


    CREATE OBJECT lo_exception.
    CONSTANTS : c_zeng_load      TYPE nrobj      VALUE 'ZENG_LOAD' .


    SELECT  *  FROM zcrossref_sto INTO TABLE  @DATA(lt_crossref)  WHERE  sursys  =  '5'  .                   "   get dump  from zcrossref_stpo



    IF is_ebrheader-ebrheadlotnav IS NOT INITIAL .
      SELECT  * FROM  zebr_t_lotinfo  INTO TABLE @DATA(lt_lotinfo_plan)
            FOR ALL ENTRIES IN @is_ebrheader-ebrheadlotnav   WHERE req_no  =  @is_ebrheader-ebrheadlotnav-req_no
                                                             AND  output_part = @is_ebrheader-ebrheadlotnav-output_part.
      DELETE  lt_lotinfo_plan  WHERE planning_id  EQ space .
      IF lt_lotinfo_plan IS NOT INITIAL.
        SELECT planning_id    ,
               release_type   ,
               licha          ,
               load_date  FROM zscm_load_plan  INTO TABLE @DATA(lt_scm_load)
                          FOR ALL ENTRIES IN @lt_lotinfo_plan  WHERE planning_id   =  @lt_lotinfo_plan-planning_id .





      ENDIF.
      " get all input materials for lot's
      SELECT a~matnr, a~mtart , a~bismt ,b~charg, b~licha INTO TABLE @DATA(lt_mch1)  FROM mara AS a INNER JOIN mch1 AS b
                                                   ON a~matnr   =  b~matnr INNER JOIN mchb AS c ON
                                                      b~matnr   =  c~matnr
                                                  AND b~charg   =  c~charg
                                                 INNER JOIN t320 AS d
                                                  ON    c~werks  =  d~werks
                                                  AND   c~lgort  =  d~lgort
                                                  FOR ALL ENTRIES IN @is_ebrheader-ebrheadlotnav
                                                  WHERE  b~licha   =  @is_ebrheader-ebrheadlotnav-lot_no
                                                   AND   a~mtart  IN  ( 'ZSRT'  , 'ZFAB'  )
                                                   AND   c~clabs  GT  0 .                                " considerig  both zsrt and lot's

      IF sy-subrc EQ 0.
        " assuming all lot will be of material type because validation is done in validate_ebr method
        READ TABLE  lt_mch1 INTO DATA(ls_mch1_mtart)   INDEX 1 .
        IF sy-subrc EQ 0.
          CLEAR lv_input_mtart .
          lv_input_mtart   = ls_mch1_mtart-mtart.
          CLEAR ls_mch1_mtart .
        ENDIF.

        " lqua
        SELECT lgnum , matnr  ,
               werks , charg  ,
              lgort
         FROM lqua
         INTO TABLE @DATA(lt_lqua)
            FOR ALL ENTRIES IN  @lt_mch1
        WHERE matnr EQ @lt_mch1-matnr
          AND charg EQ @lt_mch1-charg
          AND lgtyp EQ '003'
          AND verme GT 0.


        " mchb quantity

        SELECT matnr  , charg  ,
               werks  , lgort FROM mchb
                              INTO TABLE @DATA(lt_mchb)
                              FOR ALL ENTRIES IN @lt_mch1
                              WHERE matnr EQ @lt_mch1-matnr
                                AND charg EQ @lt_mch1-charg
                                AND clabs GT 0.
        IF sy-subrc EQ 0.
          SORT lt_mchb.
          SELECT werks ,  lgort  , lgnum
            FROM t320  INTO TABLE @DATA(lt_t320)
            FOR ALL ENTRIES IN @lt_mchb
           WHERE werks EQ @lt_mchb-werks
             AND lgort EQ @lt_mchb-lgort  .
          IF sy-subrc  EQ 0 .
            SORT  lt_t320  BY  werks  lgort .
          ENDIF.
        ENDIF.



      ENDIF.

      " get all out put material
      " output material type
      SELECT  matnr  , mtart  , bismt , wrkst ,mfrpn   FROM mara  INTO TABLE @DATA(lt_lot_mara)
                                                  FOR ALL ENTRIES IN @is_ebrheader-ebrheadlotnav
                                                   WHERE matnr  = @is_ebrheader-ebrheadlotnav-output_part .


    ENDIF.

    SELECT  a~req_no        ,
            a~lot_no        ,
            a~matnr         ,
            a~bom_no        ,
            a~bom_alt       ,
            a~route_number  ,
            a~route_counter ,
            a~old_route     ,
            a~material_status,
            b~mtart            INTO TABLE @DATA(lt_mastdata)   FROM  zebr_t_mast_data   AS a INNER JOIN  mara AS b
                                                                 ON  a~matnr     = b~matnr
                                                              WHERE  a~req_no    =   @is_ebrheader-req_no .




    " header fields
*    ls_load_plan-req_status         = is_ebrheader-req_status_no  .
    ls_load_plan-req_name           = is_ebrheader-created_by.               " requestor
    ls_load_plan-log_type           = 'RELEASE' .
    ls_load_plan-load_date          = sy-datum  .
    " Begin of change JGAND37207--PRJ#37574
    IF ( is_ebrheader-req_type EQ 'AECQ' OR is_ebrheader-req_type EQ 'PKGQ' OR is_ebrheader-req_type EQ 'FEQ' ) AND is_ebrheader-relid IS NOT INITIAL.
      ls_load_plan-build_code         = is_ebrheader-req_type  .
    ELSE.
      " End of change JGAND37207--PRJ#37574
      IF is_ebrheader-sample_marking  EQ 'X'  .
        ls_load_plan-build_code  =  'DOE' .
      ELSE.
        IF is_ebrheader-req_type   EQ 'NPI' .
          ls_load_plan-build_code    =  'NP' .
        ELSEIF  ls_load_plan-build_code    =  'PKPQ' .
          ls_load_plan-build_code    =  'PKGQ' .
        ELSE .
          ls_load_plan-build_code         = is_ebrheader-req_type  .                                                  "  for NPI   pas Np as request  type  for backend .
        ENDIF.
      ENDIF.
    ENDIF.
    ls_load_plan-final_ship_to      = is_ebrheader-ship_to .
*    ls_load_plan-ship_form          = is_ebrheader-dc_location  .
    ls_load_plan-processing_flag    = 'Y'  .
    ls_load_plan-ernam              = is_ebrheader-npp_processor.
    ls_load_plan-rel_id             = is_ebrheader-relid.
    ls_load_plan-trans_type         = '01'  .
    ls_load_plan-eng_rel            = 'Y' .
    ls_load_plan-ebr_num            = is_ebrheader-req_no .
    ls_load_plan-sample_marking     = is_ebrheader-sample_marking .
    ls_load_plan-eng_rel            = 'X'  .
    ls_load_plan-pcn_no             = is_ebrheader-pcn_no .
    " special indicator
    IF is_ebrheader-hybrid  IS NOT INITIAL .
      ls_load_plan-special_indc   =   '13'  .
*    ELSEIF is_ebrheader-mpw_wafer  IS NOT INITIAL .

    ELSE.
      ls_load_plan-special_indc   =   '11'  .
    ENDIF.

    " release type
    " output material type
    SELECT SINGLE matnr  , mtart  , bismt ,mfrpn   FROM mara  INTO @DATA(ls_out_mara)
                                                    WHERE matnr  = @is_ebrheader-output_part .


    " input mateiral  type assuming all materils will  have same  material type
    " validating all inpit materil will be sort

    " determine  release type based output type material and input material
    IF ( ls_out_mara-mtart     =  'FERT'  OR   ls_out_mara-mtart     =  'ZTST'  ) AND lv_input_mtart  =  'ZSRT'  .
      IF ls_out_mara-bismt+0(2) EQ '87'  .
        ls_load_plan-release_type   =  'DIE BANK TO PRETEST'  .
      ELSE.
        ls_load_plan-release_type   =  'DIE BANK'  .
      ENDIF.
      ls_load_plan-primary_uom_in     = 'DIE'.
*    ELSEIF ls_out_mara-mtart     =  'ZTST'  AND lv_input_mtart  =  'ZSRT'  .
*      ls_load_plan-release_type   =  'DIE BANK'  .
*      ls_load_plan-primary_uom_in     = 'DIE'.
    ELSEIF ( ls_out_mara-mtart     =  'ZBMP'   OR ls_out_mara-mtart     =  'ZPBS' ) AND lv_input_mtart  =  'ZSRT'  .
      ls_load_plan-release_type   =  'DB TO ADB'  .
      ls_load_plan-primary_uom_in     = 'DIE'.
    ELSEIF  ls_out_mara-mtart     =  'ZASY' AND lv_input_mtart  =  'ZSRT'  .
      ls_load_plan-release_type   =  'DIE BANK TO PRETEST'  .
      ls_load_plan-primary_uom_in     = 'DIE'.
    ELSEIF ( ls_out_mara-mtart     =  'FERT'   OR ls_out_mara-mtart     =  'ZTST'  ) AND lv_input_mtart  =  'ZFAB'  .
      ls_load_plan-release_type   =  'WAFER BANK '  .
      ls_load_plan-primary_uom_in     = 'DIE'.
      ls_load_plan-secondary_uom_in   =  'WFR' .
    ELSEIF  ls_out_mara-mtart     =  'ZSRT'  AND lv_input_mtart  =  'ZFAB'  .
      ls_load_plan-release_type           =  'WAFER BANK TO DIE BANK'  .
      ls_load_plan-primary_uom_in         = 'WFR'.
      ls_load_plan-secondary_uom_in       = 'WFR' .
    ELSEIF ( ls_out_mara-mtart     =  'ZBMP'   OR ls_out_mara-mtart     =  'ZPBS' ) AND lv_input_mtart  =  'ZFAB'  .
      ls_load_plan-release_type           =  'WB TO ADB'  .
      ls_load_plan-primary_uom_in         = 'DIE'.
      ls_load_plan-secondary_uom_in       =  'WFR' .
    ENDIF.


    DATA(lt_lotinfo)   =  is_ebrheader-ebrheadlotnav  .
    SORT lt_lotinfo  BY output_part  .

    LOOP AT lt_lotinfo INTO   DATA(ls_lotinfo)  .

      DATA(lv_start_date)   =  sy-datum  .
      DATA(lv_op_dietype)   = ls_lotinfo-output_die_type .
      DATA(lv_op_dierev)    = ls_lotinfo-output_die_rev   .

      AT NEW  output_part .
        CLEAR : l_plan_id  ,
                lv_planning_id   ,
                ls_load_plan-zdie_type ,
                ls_load_plan-zdie_revcode    .
        READ TABLE lt_lotinfo_plan  INTO  DATA(ls_lotinfo_plan) WITH KEY req_no       =  ls_lotinfo-req_no
                                                                         output_part  = ls_lotinfo-output_part  .
        IF sy-subrc  NE  0.                                                         " checking whether planning id already  generated , this case is with status '98'
          " get data from number range object
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '01'
              object                  = c_zeng_load         "ZBUILD_NR
            IMPORTING
              number                  = l_plan_id
            EXCEPTIONS
              interval_not_found      = 1
              number_range_not_intern = 2
              object_not_found        = 3
              quantity_is_0           = 4
              quantity_is_not_1       = 5
              interval_overflow       = 6
              buffer_overflow         = 7
              OTHERS                  = 8.
          IF sy-subrc = 0.
            CONCATENATE sy-datum+2(6) l_plan_id INTO lv_planning_id .
          ENDIF.
        ELSE.
          lv_planning_id  =  ls_lotinfo_plan-planning_id .
          READ TABLE lt_scm_load  INTO DATA(ls_scm_load_plan)  WITH KEY planning_id  = lv_planning_id  .
          IF sy-subrc EQ 0.
            ls_load_plan-load_date  = ls_scm_load_plan-load_date .
          ELSE.
            ls_load_plan-load_date  = sy-datum .
          ENDIF .
        ENDIF.
        ls_load_plan-planning_id = lv_planning_id  .
        SELECT  SINGLE pdietype , pdierev FROM   zmm_phantom_die  INTO  @DATA(ls_phantom) "#EC CI_ALL_FIELDS_NEEDED
                                                     WHERE  pdietype  = @lv_op_dietype
                                                       AND  pdierev   = @lv_op_dierev  .
        IF sy-subrc EQ 0 .
          ls_load_plan-zdie_type     = lv_op_dietype.
          ls_load_plan-zdie_revcode  = lv_op_dierev .
          CLEAR :  ls_phantom .
        ENDIF.

      ENDAT .
      IF lv_input_mtart  = 'ZFAB'  .
        ls_load_plan-qty_secndary_in         =  ls_lotinfo-wafer_qty  .
        ls_load_plan-qty_secndary_op         =  ls_lotinfo-lot_start_qty  .
      ENDIF.

      ls_load_plan-qty_primary_in         =  ls_lotinfo-lot_start_qty  .
      ls_load_plan-qty_primary_op         =  ls_lotinfo-lot_start_qty  .
      ls_load_plan-primary_uom_op         = 'EA'  .
      ls_load_plan-licha                 =  ls_lotinfo-lot_no .
*      SHIFT ls_lotinfo-output_part LEFT DELETING LEADING '0' .
      READ TABLE  lt_lot_mara  INTO DATA(ls_lot_mara)  WITH KEY matnr  = ls_lotinfo-output_part .
      IF sy-subrc EQ 0.

        CALL METHOD zcl_zebr_form_dpc_ext=>get_mat_characteristics
          EXPORTING
            iv_matnr           = ls_lot_mara-matnr
          IMPORTING
            et_allocvaluesnum  = DATA(et_allocvaluesnum)
            et_allocvalueschar = DATA(et_allocvalueschar)
            et_allocvaluescurr = DATA(et_allocvaluescurr)
            et_return          = DATA(et_return).
        READ TABLE et_allocvalueschar   INTO DATA(ls_char)   WITH KEY   charact  =  'IDENTIFIER'  .
        IF sy-subrc  EQ 0 AND ls_char-value_neutral  EQ 'M' AND ls_lot_mara-mtart  = 'FERT' .
          " if it is maxcim explode BOM and get next highest material .
*          CALL METHOD zcl_bom_explode_util=>bom_explode
*            EXPORTING
*              iv_matnr              = ls_lot_mara-matnr
*              iv_werks              = '4900'
**             ls_stpo               =
*              iv_get_marterial_type = 'X'
***             iv_active_bom         =
**             iv_bom_alt            = ls_stpo_bom-alt
*              iv_active_bom_01      = 'X'
**              iv_usage_probability  = 'X'
*            CHANGING
*              it_stpo               = lt_stpo_bom_max.
*          "  below case can have multiple materials   but all materials will have same old material number
*          "  so reading one material .
*          READ TABLE lt_stpo_bom_max  INTO  DATA(ls_stpo_bom_max)  WITH KEY  higher_matnr   =  ls_lot_mara-matnr  .
*          IF sy-subrc  EQ 0.
          DATA(lt_mastdata_mxm)  =  it_ebr_mast_data   .
          " EBr mast data can contain multiple output materials.
          DELETE  lt_mastdata_mxm WHERE  output_matnr  NE  ls_lotinfo-output_part  .
          DELETE  lt_mastdata_mxm WHERE mtart  EQ  'FERT'  .
*          SELECT SINGLE matnr , bismt FROM mara  INTO @DATA(ls_mara_max) WHERE matnr = @ls_stpo_bom_max-matnr .
          SELECT  matnr , bismt ,mtart FROM mara  INTO TABLE  @DATA(lt_mara_mxm)
                                           FOR ALL ENTRIES IN  @lt_mastdata_mxm
                                                        WHERE matnr = @lt_mastdata_mxm-matnr
                                                          AND mtart IN ('ZSRT','ZPBS','ZASY','ZTST','ZBMP').

          IF sy-subrc EQ 0.

            READ TABLE  lt_mara_mxm  INTO  DATA(ls_mara_mxm)  INDEX 1 .
            IF sy-subrc EQ 0 .
              ls_load_plan-output_material  = ls_mara_mxm-bismt .
              IF ls_load_plan-output_material+0(2)  =  '87' .
                READ TABLE  lt_mara_mxm   TRANSPORTING NO FIELDS   WITH KEY  mtart  = 'ZASY' .
                IF sy-subrc  EQ 0   AND   lv_input_mtart  =  'ZSRT'  .
                  ls_load_plan-release_type   =  'DIE BANK TO PRETEST'  .
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
*          ENDIF.

          CLEAR : ls_char ,
                  lt_mara_mxm[] ,
                  ls_mara_mxm .
        ELSEIF  ls_char-value_neutral  EQ 'C'  AND ls_lot_mara-wrkst IS NOT INITIAL .                       "  if outputmateril is comets part
          ls_load_plan-output_material        =  ls_lot_mara-wrkst.
        ELSE.
          ls_load_plan-output_material        =  ls_lot_mara-bismt .
        ENDIF .                                                        " output material  .
        REFRESH : lt_stpo_bom_max    ,
                  et_allocvaluesnum  ,
                  et_allocvalueschar ,
                  et_allocvaluescurr ,
                  et_return .




        CLEAR  : ls_lot_mara  .
      ENDIF.

      READ TABLE  lt_mch1  INTO DATA(ls_mch1)  WITH KEY  licha   = ls_lotinfo-lot_no  .       " input material  is always sort
      IF sy-subrc EQ 0.
        ls_load_plan-input_material        =   ls_mch1-bismt .
        " ship  from
        READ TABLE  lt_lqua  INTO DATA(ls_lqua)  WITH   KEY matnr   =    ls_mch1-matnr
                                                            charg   =    ls_mch1-charg .
        IF sy-subrc EQ   0  .
          SELECT SINGLE * FROM  tvarvc INTO @DATA(ls_tvarvc) WHERE name = 'Z_STO_LGNUM'
                                              AND  low =  @ls_lqua-lgnum .
          IF sy-subrc EQ 0  .
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = ls_lqua-lgnum
              IMPORTING
                output = lv_inpnr.
          ELSE .
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = ls_lqua-lgort
              IMPORTING
                output = lv_inpnr.
          ENDIF.
          CLEAR  : ls_tvarvc  .

        ELSE.
          READ TABLE  lt_mchb  INTO DATA(ls_mchb) WITH   KEY matnr   =    ls_mch1-matnr
                                                             charg   =    ls_mch1-charg .
          IF sy-subrc EQ  0 .
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = ls_mchb-lgort
              IMPORTING
                output = lv_inpnr.
          ENDIF.
        ENDIF  .
        " get External parnter number from cross ref table
        SELECT SINGLE * FROM zcrossref_sto INTO @DATA(ls_crossref_sto) WHERE sursys = '7' "#EC CI_ALL_FIELDS_NEEDED
                                                                        AND flag   = '1'
                                                                        AND inpnr  = @lv_inpnr.
        IF sy-subrc = 0.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = ls_crossref_sto-expnr
            IMPORTING
              output = ls_load_plan-ship_form.
        ENDIF.
        CLEAR : lv_inpnr ,
                ls_crossref_sto.
      ENDIF.
      IF  lv_input_mtart  =  'ZFAB'   .
        " finish
        IF ls_lotinfo-sort_facility   IS NOT INITIAL .
          SHIFT ls_lotinfo-sort_facility  LEFT DELETING LEADING '0'  .
          CLEAR lv_inpnr .
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = ls_lotinfo-sort_facility
            IMPORTING
              output = lv_inpnr.
          READ TABLE  lt_crossref INTO  DATA(ls_cross_ref)    WITH KEY   sursys  =  '5'
                                                                flag    = '9'
                                                                inpnr   =   lv_inpnr  .
          IF sy-subrc EQ 0.
            ls_load_plan-sort_facility         =  ls_cross_ref-expnr             .
            CLEAR  ls_cross_ref .
          ELSE  .
            ls_load_plan-sort_facility         =    ls_load_plan-ship_form        .
          ENDIF.


          ls_load_plan-sort_start_dt           =  lv_start_date .
          lv_start_date                       =  lv_start_date  +  7  .
          ls_load_plan-sort_exp_fin_dt      =  lv_start_date  .
          lv_start_date                       =  lv_start_date + 2 .
          READ TABLE  lt_mastdata INTO DATA(ls_mastdata)  WITH KEY lot_no  = ls_lotinfo-lot_no
                                                                  mtart   = 'ZSRT'  .
          IF sy-subrc EQ 0.
            ls_load_plan-plnnr       = ls_mastdata-route_number .
            ls_load_plan-plnal       = ls_mastdata-route_counter .
            ls_load_plan-srt_plnnr_alt   = ls_mastdata-old_route .
            CLEAR ls_mastdata .
          ENDIF.
        ENDIF.

      ENDIF.

      " bump
      IF ls_lotinfo-bump_facility  IS NOT INITIAL .

        SHIFT ls_lotinfo-bump_facility LEFT DELETING LEADING '0'  .
        CLEAR lv_inpnr .
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_lotinfo-bump_facility
          IMPORTING
            output = lv_inpnr.

        READ TABLE  lt_crossref INTO  ls_cross_ref  WITH KEY   sursys  =  '5'
                                                                      flag    = '7'
                                                                      inpnr   = lv_inpnr   .
        IF sy-subrc EQ 0.
          ls_load_plan-bump_facility           =  ls_cross_ref-expnr             .
          CLEAR  ls_cross_ref .
        ELSE.
          ls_load_plan-bump_facility           =  ls_lotinfo-bump_facility       .
          CLEAR  ls_cross_ref .
        ENDIF.
        ls_load_plan-bump_start_dt           =  lv_start_date .
        lv_start_date                        =  lv_start_date  +  7  .
        ls_load_plan-bump_fin_dt             =  lv_start_date  .
        lv_start_date                        =  lv_start_date + 2 .
      ENDIF.
      " assembly
      IF ls_lotinfo-asy_facility   IS NOT INITIAL .
        SHIFT ls_lotinfo-asy_facility  LEFT DELETING LEADING '0'  .
        CLEAR lv_inpnr .
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_lotinfo-asy_facility
          IMPORTING
            output = lv_inpnr.
        READ TABLE  lt_crossref INTO  ls_cross_ref   WITH KEY   sursys  =  '5'
                                                                      flag    = '3'
                                                                      inpnr   = lv_inpnr .
        IF sy-subrc EQ 0.
          ls_load_plan-asy_facility          =  ls_cross_ref-expnr             .
          CLEAR  ls_cross_ref .
        ELSE.
          ls_load_plan-asy_facility           =  ls_lotinfo-asy_facility       .
        ENDIF.
        ls_load_plan-asy_start_dt           =  lv_start_date .
        lv_start_date                       =  lv_start_date  +  7  .
        ls_load_plan-asy_exp_fin_dt             =  lv_start_date  .
        lv_start_date                       =  lv_start_date + 2 .
      ENDIF.
      " pbs
      IF ls_lotinfo-pbs_facility   IS NOT INITIAL .
        SHIFT ls_lotinfo-pbs_facility  LEFT DELETING LEADING '0'  .
        CLEAR lv_inpnr .
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_lotinfo-pbs_facility
          IMPORTING
            output = lv_inpnr.


        READ TABLE  lt_crossref INTO  ls_cross_ref   WITH KEY   sursys  =  '5'
                                                                      flag    = '8'
                                                                      inpnr   = lv_inpnr  .
        IF sy-subrc EQ 0.
          ls_load_plan-pbs_facility           =  ls_cross_ref-expnr             .
          CLEAR  ls_cross_ref .
        ELSE.
          READ TABLE  lt_crossref INTO  ls_cross_ref   WITH KEY   sursys  =  '5'
                                                                      flag    = 'I'
                                                                      inpnr   = lv_inpnr  .
          IF sy-subrc EQ 0.
            ls_load_plan-pbs_facility           =  ls_cross_ref-expnr             .
            CLEAR  ls_cross_ref .
          ELSE.
            ls_load_plan-pbs_facility            =  ls_lotinfo-pbs_facility       .
            CLEAR  ls_cross_ref .
          ENDIF.
        ENDIF.

        ls_load_plan-pbs_start_dt           =  lv_start_date .
        lv_start_date                       =  lv_start_date  +  7  .
        ls_load_plan-pbs_fin_dt             =  lv_start_date  .
        lv_start_date                       =  lv_start_date + 2 .
        READ TABLE  lt_mastdata INTO ls_mastdata WITH KEY lot_no  = ls_lotinfo-lot_no
                                                                 mtart   = 'ZPBS'  .
        IF sy-subrc EQ 0.
          ls_load_plan-pbs_plnnr  = ls_mastdata-route_number .
          ls_load_plan-pbs_plnal  = ls_mastdata-route_counter .
          ls_load_plan-pbs_plnnr_alt   = ls_mastdata-old_route .
          CLEAR ls_mastdata .
        ENDIF.
      ELSEIF ls_lotinfo-pbs_facility   IS  INITIAL  AND  ls_lotinfo-finish_facility   IS NOT INITIAL   AND  ls_lotinfo-bump_facility   IS NOT INITIAL .
        ls_lotinfo-pbs_facility = ls_lotinfo-finish_facility .
        SHIFT ls_lotinfo-pbs_facility  LEFT DELETING LEADING '0'  .
        CLEAR lv_inpnr .
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_lotinfo-pbs_facility
          IMPORTING
            output = lv_inpnr.


        READ TABLE  lt_crossref INTO  ls_cross_ref   WITH KEY   sursys  =  '5'
                                                                      flag    = '8'
                                                                      inpnr   = lv_inpnr  .
        IF sy-subrc EQ 0.
          ls_load_plan-pbs_facility           =  ls_cross_ref-expnr             .
          CLEAR  ls_cross_ref .
        ELSE.
          READ TABLE  lt_crossref INTO  ls_cross_ref   WITH KEY   sursys  =  '5'
                                                                      flag    = 'I'
                                                                      inpnr   = lv_inpnr  .
          IF sy-subrc EQ 0.
            ls_load_plan-pbs_facility           =  ls_cross_ref-expnr             .
            CLEAR  ls_cross_ref .
          ELSE.
            ls_load_plan-pbs_facility            =  ls_lotinfo-pbs_facility       .
            CLEAR  ls_cross_ref .
          ENDIF.
        ENDIF.

        ls_load_plan-pbs_start_dt           =  lv_start_date .
        lv_start_date                       =  lv_start_date  +  7  .
        ls_load_plan-pbs_fin_dt             =  lv_start_date  .
        lv_start_date                       =  lv_start_date + 2 .
        READ TABLE  lt_mastdata INTO ls_mastdata WITH KEY lot_no  = ls_lotinfo-lot_no
                                                                 mtart   = 'FERT'  .
        IF sy-subrc EQ 0.
          ls_load_plan-pbs_plnnr  = ls_mastdata-route_number .
          ls_load_plan-pbs_plnal  = ls_mastdata-route_counter .
          ls_load_plan-pbs_plnnr_alt   = ls_mastdata-old_route .
          CLEAR ls_mastdata .
        ENDIF .

      ENDIF.
      IF   ls_load_plan-release_type   NE  'DIE BANK TO PRETEST'    .
        " final test
        IF ls_lotinfo-final_tst_facility   IS NOT INITIAL .
          SHIFT ls_lotinfo-final_tst_facility  LEFT DELETING LEADING '0'  .
          CLEAR lv_inpnr .
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = ls_lotinfo-final_tst_facility
            IMPORTING
              output = lv_inpnr.
          READ TABLE  lt_crossref INTO ls_cross_ref   WITH KEY   sursys  =  '5'
                                                                flag    = '5'
                                                                inpnr   = lv_inpnr .
          IF sy-subrc EQ 0.
            ls_load_plan-final_tst_facility          =  ls_cross_ref-expnr             .
            CLEAR  ls_cross_ref .
          ELSE.
            READ TABLE  lt_crossref INTO  ls_cross_ref   WITH KEY   sursys  =  '5'
                                                        flag    = 'I'
                                                        inpnr   = lv_inpnr .
            IF sy-subrc EQ 0.
              ls_load_plan-final_tst_facility           =  ls_cross_ref-expnr             .
              CLEAR  ls_cross_ref .
            ELSE.
              ls_load_plan-final_tst_facility      =  ls_lotinfo-final_tst_facility       .
            ENDIF.

          ENDIF.

          ls_load_plan-final_tst_st_dt          =  lv_start_date .
          lv_start_date                        =  lv_start_date  +  7 .
          ls_load_plan-final_exp_fin_dt        =  lv_start_date  .
          lv_start_date                       =  lv_start_date + 2 .

          READ TABLE  lt_mastdata INTO ls_mastdata  WITH KEY lot_no  = ls_lotinfo-lot_no
                                                           mtart   = 'ZTST'  .
          IF sy-subrc EQ 0.
            ls_load_plan-ft_plnnr  = ls_mastdata-route_number .
            ls_load_plan-ft_plnal  = ls_mastdata-route_counter .
            ls_load_plan-ft_plnnr_alt   = ls_mastdata-old_route .
            CLEAR ls_mastdata .
          ENDIF.
          " when final test  is empty   and finish facility is avaibale thean final testa nd finish facility are same
        ELSEIF ls_lotinfo-final_tst_facility   IS  INITIAL  AND  ls_lotinfo-finish_facility   IS NOT INITIAL   AND  ls_lotinfo-asy_facility   IS NOT INITIAL .
          ls_lotinfo-final_tst_facility = ls_lotinfo-finish_facility .
          SHIFT ls_lotinfo-final_tst_facility  LEFT DELETING LEADING '0'  .
          CLEAR lv_inpnr .
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = ls_lotinfo-final_tst_facility
            IMPORTING
              output = lv_inpnr.
          READ TABLE  lt_crossref INTO ls_cross_ref   WITH KEY   sursys  =  '5'
                                                                flag    = '6'
                                                                inpnr   = lv_inpnr .
          IF sy-subrc EQ 0.
            ls_load_plan-final_tst_facility          =  ls_cross_ref-expnr             .
            CLEAR  ls_cross_ref .
          ELSE.
            READ TABLE  lt_crossref INTO  ls_cross_ref   WITH KEY   sursys  =  '5'
                                                        flag    = 'I'
                                                        inpnr   = lv_inpnr .
            IF sy-subrc EQ 0.
              ls_load_plan-final_tst_facility           =  ls_cross_ref-expnr             .
              CLEAR  ls_cross_ref .
            ELSE.
              ls_load_plan-final_tst_facility      =  ls_lotinfo-final_tst_facility       .
            ENDIF.

          ENDIF.

          ls_load_plan-final_tst_st_dt          =  lv_start_date .
          lv_start_date                        =  lv_start_date  +  7 .
          ls_load_plan-final_exp_fin_dt        =  lv_start_date  .
          lv_start_date                       =  lv_start_date + 2 .

          READ TABLE  lt_mastdata INTO ls_mastdata  WITH KEY lot_no  = ls_lotinfo-lot_no
                                                           mtart   = 'FERT'  .
          IF sy-subrc EQ 0.
            ls_load_plan-ft_plnnr  = ls_mastdata-route_number .
            ls_load_plan-ft_plnal  = ls_mastdata-route_counter .
            ls_load_plan-ft_plnnr_alt   = ls_mastdata-old_route .
            CLEAR ls_mastdata .
          ENDIF.

        ENDIF.
      ENDIF .



      IF  ls_load_plan-release_type   NE  'DIE BANK TO PRETEST'  .
        " finish
        IF ls_lotinfo-finish_facility   IS NOT INITIAL .
          SHIFT ls_lotinfo-finish_facility  LEFT DELETING LEADING '0'  .
          CLEAR lv_inpnr .
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = ls_lotinfo-finish_facility
            IMPORTING
              output = lv_inpnr.
          READ TABLE  lt_crossref INTO  ls_cross_ref    WITH KEY   sursys  =  '5'
                                                                flag    = '6'
                                                                inpnr   =   lv_inpnr  .
          IF sy-subrc EQ 0.
            ls_load_plan-finish_facility         =  ls_cross_ref-expnr             .
            CLEAR  ls_cross_ref .
          ELSE.
            READ TABLE  lt_crossref INTO ls_cross_ref   WITH KEY   sursys  =  '5'
                                                        flag    = 'I'
                                                        inpnr   =    lv_inpnr  .
            IF sy-subrc EQ 0.
              ls_load_plan-finish_facility        =  ls_cross_ref-expnr             .
              CLEAR  ls_cross_ref .
            ELSE.
              ls_load_plan-finish_facility         =  ls_lotinfo-finish_facility  .
            ENDIF.

          ENDIF.


          ls_load_plan-finish_st_dt           =  lv_start_date .
          lv_start_date                       =  lv_start_date  +  7  .
          ls_load_plan-finish_exp_fin_dt      =  lv_start_date  .
          lv_start_date                       =  lv_start_date + 2 .
          READ TABLE  lt_mastdata INTO ls_mastdata  WITH KEY lot_no  = ls_lotinfo-lot_no
                                                                  mtart   = 'FERT'  .
          IF sy-subrc EQ 0.
            ls_load_plan-fin_plnnr  = ls_mastdata-route_number .
            ls_load_plan-fin_plnal  = ls_mastdata-route_counter .
            ls_load_plan-fin_plnnr_alt   = ls_mastdata-old_route .
            CLEAR ls_mastdata .
          ENDIF.
        ENDIF.

      ENDIF.
      " die coat
      IF ls_lotinfo-die_coat   IS NOT INITIAL .
        SHIFT ls_lotinfo-die_coat LEFT DELETING LEADING '0'  .
        CLEAR lv_inpnr .
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_lotinfo-die_coat
          IMPORTING
            output = lv_inpnr.
        READ TABLE  lt_crossref INTO ls_cross_ref   WITH KEY   sursys  =  '5'
                                                 flag    = 'D'
                                                 inpnr   =    lv_inpnr  .
        IF sy-subrc EQ 0.
          ls_load_plan-dc_facility         =  ls_cross_ref-expnr             .
          CLEAR  ls_cross_ref .
        ELSE.

          ls_load_plan-dc_facility             =  ls_lotinfo-die_coat  .
        ENDIF.
      ENDIF.

      CHECK NOT is_ebrheader-req_no  IS INITIAL.
      lv_name = sy-mandt && is_ebrheader-req_no .

      ls_load_plan-db_instr =  read_text(                              " assembly instr
          EXPORTING
            id        = zcl_zebr_form_mpc_ext=>gc_tdid_die_instr    " Text ID
            name      = lv_name    " Name
            object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

      " wafer numbers should be concatenated before  die bank intructions
      " check  if if already  wafer are  available  by checking whether string contains "//"  the characters before  "//" are wafer numbers

      DATA(lv_wafer_no)   = ls_lotinfo-wafer_no  .
      REPLACE ALL OCCURRENCES OF  '"'   IN  lv_wafer_no WITH space .

      REPLACE ALL  OCCURRENCES OF ','  IN lv_wafer_no  WITH '-'  .

      CONDENSE lv_wafer_no .

      IF ls_load_plan-db_instr CS '//'   .
        ls_load_plan-db_instr   =  lv_wafer_no    && ls_load_plan-db_instr+sy-fdpos .
      ELSE .
        ls_load_plan-db_instr   =  lv_wafer_no    &&  '//'   && ' ' && ls_load_plan-db_instr .
      ENDIF.

      ls_load_plan-sort_instr =  read_text(                              " assembly instr
      EXPORTING
        id        = zcl_zebr_form_mpc_ext=>gc_tdid_srt_instr    " Text ID
        name      = lv_name    " Name
        object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

      ls_load_plan-splins_text =  read_text(                              " assembly instr
          EXPORTING
            id        = zcl_zebr_form_mpc_ext=>gc_tdid_assy_instr    " Text ID
            name      = lv_name    " Name
            object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

      ls_load_plan-splins_text2 =  read_text(
            EXPORTING
              id        = zcl_zebr_form_mpc_ext=>gc_tdid_bump_instr    " Text ID
              name      = lv_name    " Name
              object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object
      ls_load_plan-splins_text4 =  read_text(
         EXPORTING
           id        = zcl_zebr_form_mpc_ext=>gc_tdid_test_instr    " Text ID
           name      = lv_name    " Name
           object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

      ls_load_plan-splins_text3 =  read_text(
          EXPORTING
            id        = zcl_zebr_form_mpc_ext=>gc_tdid_pbs_instr    " Text ID
            name      = lv_name    " Name
            object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

      ls_load_plan-splins_text5 =  read_text(
        EXPORTING
          id        = zcl_zebr_form_mpc_ext=>gc_tdid_fert_instr    " Text ID
          name      = lv_name    " Name
          object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

      APPEND INITIAL LINE TO lt_ebrlot  ASSIGNING FIELD-SYMBOL(<fs_ebrlot>)  .
      MOVE-CORRESPONDING  ls_lotinfo  TO  <fs_ebrlot>   .
      <fs_ebrlot>-planning_id  = lv_planning_id .


      APPEND  ls_load_plan  TO lt_load_plan .
      CLEAR : ls_load_plan-licha           ,
              ls_load_plan-qty_primary_in  ,
              ls_load_plan-qty_primary_op  ,
              ls_load_plan-primary_uom_op   ,
              ls_load_plan-output_material  ,
              ls_load_plan-output_material ,
              ls_load_plan-input_material  ,
              ls_load_plan-dc_facility    ,
              ls_load_plan-final_tst_facility,
              ls_load_plan-pbs_facility,
              ls_load_plan-finish_facility,
              ls_load_plan-asy_facility ,
              ls_load_plan-ft_plnnr,
              ls_load_plan-ft_plnal,
              ls_load_plan-bump_facility  ,
              ls_load_plan-bump_start_dt  ,
              ls_load_plan-bump_fin_dt  ,
              ls_load_plan-dc_facility,
              ls_load_plan-qty_secndary_in ,
              ls_load_plan-qty_secndary_op  ,
              ls_load_plan-fin_plnnr  ,
              ls_load_plan-fin_plnal  ,
              ls_load_plan-fin_plnnr_alt,
              ls_load_plan-pbs_plnnr  ,
              ls_load_plan-pbs_plnal  ,
              ls_load_plan-pbs_plnnr_alt,
              ls_load_plan-plnnr  ,
              ls_load_plan-plnal  ,
              ls_load_plan-ft_plnnr  ,
              ls_load_plan-ft_plnal  ,
              ls_load_plan-ft_plnnr_alt,
              ls_load_plan-srt_plnnr_alt,
              ls_load_plan-finish_st_dt,
              ls_load_plan-finish_exp_fin_dt ,
              ls_load_plan-final_tst_st_dt   ,
              ls_load_plan-final_exp_fin_dt  ,
              ls_load_plan-sort_start_dt        ,
              ls_load_plan-sort_exp_fin_dt   ,
              ls_load_plan-pbs_start_dt        ,
              ls_load_plan-pbs_fin_dt          ,
              ls_load_plan-asy_start_dt        ,
              ls_load_plan-asy_exp_fin_dt      ,
              ls_load_plan-licha               ,
              ls_load_plan-ship_form           ,
              lv_op_dierev                     ,
              lv_op_dietype ,
              lv_wafer_no.
    ENDLOOP  .

    IF lt_load_plan IS NOT INITIAL .
      " delte entries  in zscm_load_plan before updating  zscm_lload_plan for that EBR number
      DELETE FROM zscm_load_plan  WHERE ebr_num  =  is_ebrheader-req_no  . "#EC CI_NOFIELD
      IF sy-subrc EQ 0 .
        COMMIT WORK .
      ENDIF.
      MODIFY zscm_load_plan FROM TABLE lt_load_plan .
      IF sy-subrc EQ 0.
        COMMIT WORK .
      ENDIF.
    ENDIF.
    IF lt_ebrlot IS NOT INITIAL .
      MODIFY  zebr_t_lotinfo  FROM TABLE  lt_ebrlot .
      IF sy-subrc EQ 0.
        COMMIT WORK .
      ENDIF.
    ENDIF.
    IF lv_planning_id IS NOT INITIAL .      " erro qty is not aviable
      " send mail
      TRY.
          CALL METHOD zcl_ebr_utility=>email_functionality
            EXPORTING
              im_ebr        = is_ebrheader-req_no
              im_req_type   = 'N'
              im_req_status = is_ebrheader-req_status_no
              im_group      = zcl_ebr_utility=>zemail_grp_sub_lp
*            IMPORTING
*             et_notif      =
            .
        CATCH cx_abap_invalid_value .
      ENDTRY.

      IF lo_exception IS BOUND.
        lo_exception->get_msg_container( )->add_message(
              EXPORTING
               iv_msg_type    = 'S'
               iv_msg_id      = '00'
               iv_msg_number  = '123'
               iv_msg_text    = 'Planning Id Successfully created '  &&  lv_planning_id ).
        RAISE EXCEPTION lo_exception.
        RETURN.
      ENDIF.

    ELSE.
      IF lo_exception IS BOUND.
        lo_exception->get_msg_container( )->add_message(
              EXPORTING
               iv_msg_type     = 'E'
               iv_msg_id      = '00'
               iv_msg_number  = '123'
               iv_msg_text    =   'Planning ID is not generated ' ).
        RAISE EXCEPTION lo_exception.
        RETURN.
      ENDIF.
    ENDIF.
    CLEAR  : ls_out_mara ,
             lv_input_mtart.


  ENDMETHOD.


  METHOD SUBMIT_EBR_SCM.
    DATA  : lt_load_plan    TYPE STANDARD TABLE OF   zscm_load_plan,
            ls_load_plan    TYPE   zscm_load_plan,
            l_plan_id       TYPE ebelp,
            lv_input_mtart  TYPE  mtart,
            lv_name         TYPE thead-tdname,
            lv_planning_id  TYPE zscm_load_plan-planning_id,
            lo_exception    TYPE REF TO /iwbep/cx_mgw_tech_exception,
            lv_inpnr        TYPE zcrossref_sto-inpnr,
            lt_ebrlot       TYPE STANDARD TABLE OF zebr_t_lotinfo,
            lt_stpo_bom_max TYPE ztt_bom_com_level.


    CREATE OBJECT lo_exception.
    CONSTANTS : c_zeng_load      TYPE nrobj      VALUE 'ZENG_LOAD' .


    SELECT  *  FROM zcrossref_sto INTO TABLE  @DATA(lt_crossref)  WHERE  sursys  =  '5'  .                   "   get dump  from zcrossref_stpo

    SELECT  *  from zebr_config  INTO TABLE @data(lt_ship_to)  where name  = 'SHIP_TO'  .
    if sy-subrc eq 0 .
      sort lt_ship_to by name   field1 .
    endif .



    IF is_ebrheader-ebrheadlotnav IS NOT INITIAL .
      SELECT  * FROM  zebr_t_lotinfo  INTO TABLE @DATA(lt_lotinfo_plan)
            FOR ALL ENTRIES IN @is_ebrheader-ebrheadlotnav   WHERE req_no  =  @is_ebrheader-ebrheadlotnav-req_no
                                                             AND  output_part = @is_ebrheader-ebrheadlotnav-output_part.
      DELETE  lt_lotinfo_plan  WHERE planning_id  EQ space .
      IF lt_lotinfo_plan IS NOT INITIAL.
        SELECT planning_id    ,
               release_type   ,
               licha          ,
               load_date  FROM zscm_load_plan  INTO TABLE @DATA(lt_scm_load)
                          FOR ALL ENTRIES IN @lt_lotinfo_plan  WHERE planning_id   =  @lt_lotinfo_plan-planning_id .





      ENDIF.
      " get all input materials for lot's
      SELECT a~matnr, a~mtart , a~mfrpn ,a~bismt ,b~charg, b~licha INTO TABLE @DATA(lt_mch1)  FROM mara AS a INNER JOIN mch1 AS b
                                                   ON a~matnr   =  b~matnr INNER JOIN mchb AS c ON
                                                      b~matnr   =  c~matnr
                                                  AND b~charg   =  c~charg
                                                  FOR ALL ENTRIES IN @is_ebrheader-ebrheadlotnav
                                                  WHERE  b~licha   =  @is_ebrheader-ebrheadlotnav-lot_no
                                                   AND   a~mtart  IN  ( 'ZSRT'  , 'ZFAB'  )
                                                   AND   c~clabs  GT  0 .                                " considerig  both zsrt and lot's

      IF sy-subrc EQ 0.
        " assuming all lot will be of material type because validation is done in validate_ebr method
        READ TABLE  lt_mch1 INTO DATA(ls_mch1_mtart)   INDEX 1 .
        IF sy-subrc EQ 0.
          CLEAR lv_input_mtart .
          lv_input_mtart   = ls_mch1_mtart-mtart.
          CLEAR ls_mch1_mtart .
        ENDIF.

        " lqua
        SELECT lgnum , matnr  ,
               werks , charg  ,
              lgort
         FROM lqua
         INTO TABLE @DATA(lt_lqua)
            FOR ALL ENTRIES IN  @lt_mch1
        WHERE matnr EQ @lt_mch1-matnr
          AND charg EQ @lt_mch1-charg
          AND lgtyp EQ '003'
          AND verme GT 0.


        " mchb quantity

        SELECT matnr  , charg  ,
               werks  , lgort FROM mchb
                              INTO TABLE @DATA(lt_mchb)
                              FOR ALL ENTRIES IN @lt_mch1
                              WHERE matnr EQ @lt_mch1-matnr
                                AND charg EQ @lt_mch1-charg
                                AND clabs GT 0.
        IF sy-subrc EQ 0.
          SORT lt_mchb.
          SELECT werks ,  lgort  , lgnum
            FROM t320  INTO TABLE @DATA(lt_t320)
            FOR ALL ENTRIES IN @lt_mchb
           WHERE werks EQ @lt_mchb-werks
             AND lgort EQ @lt_mchb-lgort  .
          IF sy-subrc  EQ 0 .
            SORT  lt_t320  BY  werks  lgort .
          ENDIF.
        ENDIF.



      ENDIF.

      " get all out put material
      " output material type
      SELECT  matnr  , mtart  , bismt ,mfrpn   FROM mara  INTO TABLE @DATA(lt_lot_mara)
                                                  FOR ALL ENTRIES IN @is_ebrheader-ebrheadlotnav
                                                   WHERE matnr  = @is_ebrheader-ebrheadlotnav-output_part .


    ENDIF.

    SELECT  a~req_no        ,
            a~lot_no        ,
            a~matnr         ,
            a~bom_no        ,
            a~bom_alt       ,
            a~route_number  ,
            a~route_counter ,
            a~old_route     ,
            a~material_status,
            b~mtart            INTO TABLE @DATA(lt_mastdata)   FROM  zebr_t_mast_data   AS a INNER JOIN  mara AS b
                                                                 ON  a~matnr     = b~matnr
                                                              WHERE  a~req_no    =   @is_ebrheader-req_no .




    " header fields
*    ls_load_plan-req_status         = is_ebrheader-req_status_no  .
    ls_load_plan-req_name           = is_ebrheader-created_by.               " requestor
    ls_load_plan-log_type           = 'END_STATE' .
    ls_load_plan-load_date          = sy-datum  .
    IF is_ebrheader-sample_marking  EQ 'X'  .
      ls_load_plan-build_code  =  'DOE' .
    ELSE.
      IF is_ebrheader-req_type   EQ 'NPI' .
        ls_load_plan-build_code    =  'NP' .
      ELSEIF  ls_load_plan-build_code    =  'PKPQ' .
        ls_load_plan-build_code    =  'PKGQ' .
      ELSE .
        ls_load_plan-build_code         = is_ebrheader-req_type  .                                                  "  for NPI   pas Np as request  type  for backend .
      ENDIF.
    ENDIF.
    " begin of  scm chnages
    read TABLE lt_ship_to  INTO data(ls_ship_to)  with key name  = 'SHIP_TO'
                                                         field1  =  is_ebrheader-ship_to   .
   if sy-subrc eq 0 .
    ls_load_plan-final_ship_to_plant      = ls_ship_to-field2 .
    ls_load_plan-final_ship_to_sloc       = ls_ship_to-field3 .

   endif.
   " end of scm changes
*    ls_load_plan-ship_form          = is_ebrheader-dc_location  .
    ls_load_plan-processing_flag    = 'Y'  .
    ls_load_plan-ernam              = is_ebrheader-npp_processor.
    ls_load_plan-rel_id             = is_ebrheader-relid.
    ls_load_plan-trans_type         = '01'  .
    ls_load_plan-eng_rel            = 'Y' .
    ls_load_plan-ebr_num            = is_ebrheader-req_no .
    ls_load_plan-sample_marking     = is_ebrheader-sample_marking .
    ls_load_plan-eng_rel            = 'X'  .
    ls_load_plan-pcn_no             = is_ebrheader-pcn_no .
    " special indicator
    IF is_ebrheader-hybrid  IS NOT INITIAL .
      ls_load_plan-special_indc   =   '13'  .
*    ELSEIF is_ebrheader-mpw_wafer  IS NOT INITIAL .

    ELSE.
      ls_load_plan-special_indc   =   '11'  .
    ENDIF.

    " release type
    " output material type
    SELECT SINGLE matnr  , mtart  , bismt ,mfrpn   FROM mara  INTO @DATA(ls_out_mara)
                                                    WHERE matnr  = @is_ebrheader-output_part .


    " input mateiral  type assuming all materils will  have same  material type
    " validating all inpit materil will be sort

    " determine  release type based output type material and input material
    " determine  release type based output type material and input material
    IF ( ls_out_mara-mtart     =  'FERT'  OR   ls_out_mara-mtart     =  'ZTST'  ) AND lv_input_mtart  =  'ZSRT'  .
      IF ls_out_mara-bismt+0(2) EQ '87'  .
        ls_load_plan-release_type   =  'DIE BANK TO PRETEST'  .
      ELSE.
        ls_load_plan-release_type   =  'DIE BANK'  .
      ENDIF.
      ls_load_plan-primary_uom_in     = 'DIE'.
    ELSEIF ls_out_mara-mtart     =  'ZTST'  AND lv_input_mtart  =  'ZSRT'  .
      ls_load_plan-release_type   =  'DIE BANK'  .
      ls_load_plan-primary_uom_in     = 'DIE'.
    ELSEIF ( ls_out_mara-mtart     =  'ZBMP'   OR ls_out_mara-mtart     =  'ZPBS' ) AND lv_input_mtart  =  'ZSRT'  .
      ls_load_plan-release_type   =  'DB TO ADB'  .
      ls_load_plan-primary_uom_in     = 'DIE'.
    ELSEIF  ls_out_mara-mtart     =  'ZASY' AND lv_input_mtart  =  'ZSRT'  .
      ls_load_plan-release_type   =  'DIE BANK TO PRETEST'  .
      ls_load_plan-primary_uom_in     = 'DIE'.
    ELSEIF ( ls_out_mara-mtart     =  'FERT'   OR ls_out_mara-mtart     =  'ZTST'  ) AND lv_input_mtart  =  'ZFAB'  .
      ls_load_plan-release_type   =  'WAFER BANK '  .
      ls_load_plan-primary_uom_in     = 'DIE'.
      ls_load_plan-secondary_uom_in   =  'WFR' .
    ELSEIF  ls_out_mara-mtart     =  'ZSRT'  AND lv_input_mtart  =  'ZFAB'  .
      ls_load_plan-release_type           =  'WAFER BANK TO DIE BANK'  .
      ls_load_plan-primary_uom_in         = 'WFR'.
      ls_load_plan-secondary_uom_in       = 'WFR' .
    ELSEIF ( ls_out_mara-mtart     =  'ZBMP'   OR ls_out_mara-mtart     =  'ZPBS' ) AND lv_input_mtart  =  'ZFAB'  .
      ls_load_plan-release_type           =  'WB TO ADB'  .
      ls_load_plan-primary_uom_in         = 'DIE'.
      ls_load_plan-secondary_uom_in       =  'WFR' .
    ENDIF.


    DATA(lt_lotinfo)   =  is_ebrheader-ebrheadlotnav  .
    SORT lt_lotinfo  BY output_part  .
*    IF lt_lotinfo IS NOT INITIAL .
*      SELECT  * FROM   zmm_phantom_die  INTO  TABLE @data(lt_phantom)  for ALL ENTRIES IN @lt_lotinfo
*                                                           WHERE  pdietype  = @lt_lotinfo-output_die_type
*                                                             AND  pdierev   = @lt_lotinfo-output_die_rev  .
*    ENDIF.
    LOOP AT lt_lotinfo INTO   DATA(ls_lotinfo)  .
      DATA(lv_start_date)   =  sy-datum  .
      AT NEW  output_part .
        CLEAR : l_plan_id  ,
                lv_planning_id .
        READ TABLE lt_lotinfo_plan  INTO  DATA(ls_lotinfo_plan) WITH KEY req_no       =  ls_lotinfo-req_no
                                                                         output_part  = ls_lotinfo-output_part  .
        IF sy-subrc  NE  0.                                                         " checking whether planning id already  generated , this case is with status '98'
          " get data from number range object
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '01'
              object                  = c_zeng_load         "ZBUILD_NR
            IMPORTING
              number                  = l_plan_id
            EXCEPTIONS
              interval_not_found      = 1
              number_range_not_intern = 2
              object_not_found        = 3
              quantity_is_0           = 4
              quantity_is_not_1       = 5
              interval_overflow       = 6
              buffer_overflow         = 7
              OTHERS                  = 8.
          IF sy-subrc = 0.
            CONCATENATE sy-datum+2(6) l_plan_id INTO lv_planning_id .
          ENDIF.
        ELSE.
          lv_planning_id  =  ls_lotinfo_plan-planning_id .
          READ TABLE lt_scm_load  INTO DATA(ls_scm_load_plan)  WITH KEY planning_id  = lv_planning_id  .
          IF sy-subrc EQ 0.
            ls_load_plan-load_date  = ls_scm_load_plan-load_date .
          ELSE.
            ls_load_plan-load_date  = sy-datum .
          ENDIF .
          SELECT  * FROM   zmm_phantom_die  INTO  TABLE @DATA(lt_phantom)  WHERE  pdietype  = @ls_lotinfo-output_die_type "#EC CI_ALL_FIELDS_NEEDED
                                                                             AND  pdierev   = @ls_lotinfo-output_die_rev  .
          IF sy-subrc EQ 0 .
            ls_load_plan-zdie_type     = ls_lotinfo-output_die_type .
            ls_load_plan-zdie_revcode  = ls_lotinfo-output_die_rev .
            REFRESH :  lt_phantom .
          ENDIF.
        ENDIF.
        ls_load_plan-planning_id = lv_planning_id  .


      ENDAT .
      IF lv_input_mtart  = 'ZFAB'  .
        ls_load_plan-qty_secndary_in         =  ls_lotinfo-wafer_qty  .
        ls_load_plan-qty_secndary_op         =  ls_lotinfo-lot_start_qty  .
      ENDIF.

      ls_load_plan-qty_primary_in         =  ls_lotinfo-lot_start_qty  .
      ls_load_plan-qty_primary_op         =  ls_lotinfo-lot_start_qty  .
      ls_load_plan-primary_uom_op         = 'EA'  .
      ls_load_plan-licha                 =  ls_lotinfo-lot_no .
*      SHIFT ls_lotinfo-output_part LEFT DELETING LEADING '0' .
      READ TABLE  lt_lot_mara  INTO DATA(ls_lot_mara)  WITH KEY matnr  = ls_lotinfo-output_part .
      IF sy-subrc EQ 0.

*        CALL METHOD zcl_zebr_form_dpc_ext=>get_mat_characteristics
*          EXPORTING
*            iv_matnr           = ls_lot_mara-matnr
*          IMPORTING
*            et_allocvaluesnum  = DATA(et_allocvaluesnum)
*            et_allocvalueschar = DATA(et_allocvalueschar)
*            et_allocvaluescurr = DATA(et_allocvaluescurr)
*            et_return          = DATA(et_return).
*        READ TABLE et_allocvalueschar   INTO DATA(ls_char)   WITH KEY   charact  =  'IDENTIFIER'  .
*        IF sy-subrc  EQ 0 AND ls_char-value_neutral  EQ 'M' AND ls_lot_mara-mtart  = 'FERT' .
*          " if it is maxcim explode BOM and get next highest material .
*          CALL METHOD zcl_bom_explode_util=>bom_explode
*            EXPORTING
*              iv_matnr              = ls_lot_mara-matnr
*              iv_werks              = '4900'
**             ls_stpo               =
*              iv_get_marterial_type = 'X'
***             iv_active_bom         =
**             iv_bom_alt            = ls_stpo_bom-alt
*              iv_active_bom_01      = 'X'
*            CHANGING
*              it_stpo               = lt_stpo_bom_max.
*          "  below case can have multiple materials   but all materials will have same old material number
*          "  so reading one material .
*          READ TABLE lt_stpo_bom_max  INTO  DATA(ls_stpo_bom_max)  WITH KEY  higher_matnr   =  ls_lot_mara-matnr  .
*          IF sy-subrc  EQ 0.
*            SELECT SINGLE matnr , bismt FROM mara  INTO @DATA(ls_mara_max) WHERE matnr = @ls_stpo_bom_max-matnr .
*            IF sy-subrc EQ 0.
*              ls_load_plan-output_material  = ls_mara_max-bismt .
*            ENDIF.
*          ENDIF.
*
*          CLEAR ls_char .
*        ELSE.
          ls_load_plan-output_material        =  ls_lot_mara-mfrpn .
          CLEAR  : ls_lot_mara  .
        ENDIF .                                                        " output material  .
*        REFRESH : lt_stpo_bom_max    ,
*                  et_allocvaluesnum  ,
*                  et_allocvalueschar ,
*                  et_allocvaluescurr ,
*                  et_return .





*      ENDIF.

      READ TABLE  lt_mch1  INTO DATA(ls_mch1)  WITH KEY  licha   = ls_lotinfo-lot_no  .       " input material  is always sort
      IF sy-subrc EQ 0.
        ls_load_plan-input_material        =   ls_mch1-matnr .
        ls_load_plan-sap_batch_number      =   ls_mch1-charg .
        " ship  from
*            READ TABLE  lt_lqua  INTO DATA(ls_lqua)  WITH   KEY matnr   =    ls_mch1-matnr
*                                                                charg   =    ls_mch1-charg .
*            IF sy-subrc EQ   0  .
*              SELECT SINGLE * FROM  tvarvc INTO @DATA(ls_tvarvc) WHERE name = 'Z_STO_LGNUM'
*                                                  AND  low =  @ls_lqua-lgnum .
*                IF sy-subrc EQ 0  .
*                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                    EXPORTING
*                      input  = ls_lqua-lgnum
*                    IMPORTING
*                      output = lv_inpnr.
*                ELSE .
*                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                    EXPORTING
*                      input  = ls_lqua-lgort
*                    IMPORTING
*                      output = lv_inpnr.
*                ENDIF.
*                CLEAR  : ls_tvarvc  .
*
*              ELSE.
*                READ TABLE  lt_mchb  INTO DATA(ls_mchb) WITH   KEY matnr   =    ls_mch1-matnr
*                                                                   charg   =    ls_mch1-charg .
*                IF sy-subrc EQ  0 .
*                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                    EXPORTING
*                      input  = ls_mchb-lgort
*                    IMPORTING
*                      output = lv_inpnr.
*                ENDIF.
*              ENDIF  .
*              " get External parnter number from cross ref table
*              SELECT SINGLE * FROM zcrossref_sto INTO @DATA(ls_crossref_sto) WHERE sursys = '7' "#EC CI_ALL_FIELDS_NEEDED
*                                                                              AND flag   = '1'
*                                                                              AND inpnr  = @lv_inpnr.
*                IF sy-subrc = 0.
*                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*                    EXPORTING
*                      input  = ls_crossref_sto-expnr
*                    IMPORTING
*                      output = ls_load_plan-ship_form.
*                ENDIF.
*                CLEAR : lv_inpnr ,
*                        ls_crossref_sto.
      "scm changes
       READ TABLE  lt_mchb  INTO DATA(ls_mchb) WITH   KEY matnr   =    ls_mch1-matnr
                                                          charg   =    ls_mch1-charg .
       IF sy-subrc EQ  0 .
               ls_load_plan-ship_from_plant  = ls_mchb-werks .
               ls_load_plan-ship_from_sloc   = ls_mchb-lgort .

       ENDIF.

      ENDIF.
      IF  lv_input_mtart  =  'ZFAB'   .
        " finish
        IF ls_lotinfo-sort_facility   IS NOT INITIAL .
*          SHIFT ls_lotinfo-sort_facility  LEFT DELETING LEADING '0'  .
*          CLEAR lv_inpnr .
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = ls_lotinfo-sort_facility
*            IMPORTING
*              output = lv_inpnr.
*          READ TABLE  lt_crossref INTO  DATA(ls_cross_ref)    WITH KEY   sursys  =  '5'
*                                                                flag    = '9'
*                                                                inpnr   =   lv_inpnr  .
*          IF sy-subrc EQ 0.
*            ls_load_plan-sort_facility         =  ls_cross_ref-expnr             .
*            CLEAR  ls_cross_ref .
*          ELSE  .
*            ls_load_plan-sort_facility         =    ls_load_plan-ship_form        .
*          ENDIF.
          ls_load_plan-sort_facility   = ls_lotinfo-sort_facility .


          ls_load_plan-sort_start_dt           =  lv_start_date .
          lv_start_date                       =  lv_start_date  +  7  .
          ls_load_plan-sort_exp_fin_dt      =  lv_start_date  .
          lv_start_date                       =  lv_start_date + 2 .
          READ TABLE  lt_mastdata INTO DATA(ls_mastdata)  WITH KEY lot_no  = ls_lotinfo-lot_no
                                                                  mtart   = 'ZSRT'  .
          IF sy-subrc EQ 0.
            ls_load_plan-plnnr       = ls_mastdata-route_number .
            ls_load_plan-plnal       = ls_mastdata-route_counter .
            ls_load_plan-srt_plnnr_alt   = ls_mastdata-old_route .
            CLEAR ls_mastdata .
          ENDIF.
        ENDIF.

      ENDIF.

      " bump
      IF ls_lotinfo-bump_facility  IS NOT INITIAL .

*        SHIFT ls_lotinfo-bump_facility LEFT DELETING LEADING '0'  .
*        CLEAR lv_inpnr .
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = ls_lotinfo-bump_facility
*          IMPORTING
*            output = lv_inpnr.
*
*        READ TABLE  lt_crossref INTO  ls_cross_ref  WITH KEY   sursys  =  '5'
*                                                                      flag    = '7'
*                                                                      inpnr   = lv_inpnr   .
*        IF sy-subrc EQ 0.
*          ls_load_plan-bump_facility           =  ls_cross_ref-expnr             .
*          CLEAR  ls_cross_ref .
*        ELSE.
*          ls_load_plan-bump_facility           =  ls_lotinfo-bump_facility       .
*          CLEAR  ls_cross_ref .
*        ENDIF.
        ls_load_plan-bump_facility   = ls_lotinfo-bump_facility .
        ls_load_plan-bump_start_dt           =  lv_start_date .
        lv_start_date                        =  lv_start_date  +  7  .
        ls_load_plan-bump_fin_dt             =  lv_start_date  .
        lv_start_date                        =  lv_start_date + 2 .
      ENDIF.
      " assembly
      IF ls_lotinfo-asy_facility   IS NOT INITIAL .
*        SHIFT ls_lotinfo-asy_facility  LEFT DELETING LEADING '0'  .
*        CLEAR lv_inpnr .
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = ls_lotinfo-asy_facility
*          IMPORTING
*            output = lv_inpnr.
*        READ TABLE  lt_crossref INTO  ls_cross_ref   WITH KEY   sursys  =  '5'
*                                                                      flag    = '3'
*                                                                      inpnr   = lv_inpnr .
*        IF sy-subrc EQ 0.
*          ls_load_plan-asy_facility          =  ls_cross_ref-expnr             .
*          CLEAR  ls_cross_ref .
*        ELSE.
*          ls_load_plan-asy_facility           =  ls_lotinfo-asy_facility       .
*        ENDIF.
        ls_load_plan-asy_facility   = ls_lotinfo-asy_facility .
        ls_load_plan-asy_start_dt           =  lv_start_date .
        lv_start_date                       =  lv_start_date  +  7  .
        ls_load_plan-asy_exp_fin_dt             =  lv_start_date  .
        lv_start_date                       =  lv_start_date + 2 .
      ENDIF.
      " pbs
      IF ls_lotinfo-pbs_facility   IS NOT INITIAL .
        SHIFT ls_lotinfo-pbs_facility  LEFT DELETING LEADING '0'  .
        CLEAR lv_inpnr .
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_lotinfo-pbs_facility
          IMPORTING
            output = lv_inpnr.

*
*        READ TABLE  lt_crossref INTO  ls_cross_ref   WITH KEY   sursys  =  '5'
*                                                                      flag    = '8'
*                                                                      inpnr   = lv_inpnr  .
*        IF sy-subrc EQ 0.
*          ls_load_plan-pbs_facility           =  ls_cross_ref-expnr             .
*          CLEAR  ls_cross_ref .
*        ELSE.
*          READ TABLE  lt_crossref INTO  ls_cross_ref   WITH KEY   sursys  =  '5'
*                                                                      flag    = 'I'
*                                                                      inpnr   = lv_inpnr  .
*          IF sy-subrc EQ 0.
*            ls_load_plan-pbs_facility           =  ls_cross_ref-expnr             .
*            CLEAR  ls_cross_ref .
*          ELSE.
*            ls_load_plan-pbs_facility            =  ls_lotinfo-pbs_facility       .
*            CLEAR  ls_cross_ref .
*          ENDIF.
*        ENDIF.
        ls_load_plan-pbs_facility           = ls_lotinfo-pbs_facility .
        ls_load_plan-pbs_start_dt           =  lv_start_date .
        lv_start_date                       =  lv_start_date  +  7  .
        ls_load_plan-pbs_fin_dt             =  lv_start_date  .
        lv_start_date                       =  lv_start_date + 2 .
        READ TABLE  lt_mastdata INTO ls_mastdata WITH KEY lot_no  = ls_lotinfo-lot_no
                                                                 mtart   = 'ZPBS'  .
        IF sy-subrc EQ 0.
          ls_load_plan-pbs_plnnr  = ls_mastdata-route_number .
          ls_load_plan-pbs_plnal  = ls_mastdata-route_counter .
          ls_load_plan-pbs_plnnr_alt   = ls_mastdata-old_route .
          CLEAR ls_mastdata .
        ENDIF.
      ELSEIF ls_lotinfo-pbs_facility   IS  INITIAL  AND  ls_lotinfo-finish_facility   IS NOT INITIAL   AND  ls_lotinfo-bump_facility   IS NOT INITIAL .
        ls_lotinfo-pbs_facility = ls_lotinfo-finish_facility .
*        SHIFT ls_lotinfo-pbs_facility  LEFT DELETING LEADING '0'  .
*        CLEAR lv_inpnr .
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = ls_lotinfo-pbs_facility
*          IMPORTING
*            output = lv_inpnr.
*
*
*        READ TABLE  lt_crossref INTO  ls_cross_ref   WITH KEY   sursys  =  '5'
*                                                                      flag    = '8'
*                                                                      inpnr   = lv_inpnr  .
*        IF sy-subrc EQ 0.
*          ls_load_plan-pbs_facility           =  ls_cross_ref-expnr             .
*          CLEAR  ls_cross_ref .
*        ELSE.
*          READ TABLE  lt_crossref INTO  ls_cross_ref   WITH KEY   sursys  =  '5'
*                                                                      flag    = 'I'
*                                                                      inpnr   = lv_inpnr  .
*          IF sy-subrc EQ 0.
*            ls_load_plan-pbs_facility           =  ls_cross_ref-expnr             .
*            CLEAR  ls_cross_ref .
*          ELSE.
*            ls_load_plan-pbs_facility            =  ls_lotinfo-pbs_facility       .
*            CLEAR  ls_cross_ref .
*          ENDIF.
*        ENDIF.
        ls_load_plan-pbs_facility           =  ls_lotinfo-pbs_facility .
        ls_load_plan-pbs_start_dt           =  lv_start_date .
        lv_start_date                       =  lv_start_date  +  7  .
        ls_load_plan-pbs_fin_dt             =  lv_start_date  .
        lv_start_date                       =  lv_start_date + 2 .
        READ TABLE  lt_mastdata INTO ls_mastdata WITH KEY lot_no  = ls_lotinfo-lot_no
                                                                 mtart   = 'FERT'  .
        IF sy-subrc EQ 0.
          ls_load_plan-pbs_plnnr  = ls_mastdata-route_number .
          ls_load_plan-pbs_plnal  = ls_mastdata-route_counter .
          ls_load_plan-pbs_plnnr_alt   = ls_mastdata-old_route .
          CLEAR ls_mastdata .
        ENDIF .

      ENDIF.

      " final test
      IF ls_lotinfo-final_tst_facility   IS NOT INITIAL .
*        SHIFT ls_lotinfo-final_tst_facility  LEFT DELETING LEADING '0'  .
*        CLEAR lv_inpnr .
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = ls_lotinfo-final_tst_facility
*          IMPORTING
*            output = lv_inpnr.
*        READ TABLE  lt_crossref INTO ls_cross_ref   WITH KEY   sursys  =  '5'
*                                                              flag    = '5'
*                                                              inpnr   = lv_inpnr .
*        IF sy-subrc EQ 0.
*          ls_load_plan-final_tst_facility          =  ls_cross_ref-expnr             .
*          CLEAR  ls_cross_ref .
*        ELSE.
*          READ TABLE  lt_crossref INTO  ls_cross_ref   WITH KEY   sursys  =  '5'
*                                                      flag    = 'I'
*                                                      inpnr   = lv_inpnr .
*          IF sy-subrc EQ 0.
*            ls_load_plan-final_tst_facility           =  ls_cross_ref-expnr             .
*            CLEAR  ls_cross_ref .
*          ELSE.
*            ls_load_plan-final_tst_facility      =  ls_lotinfo-final_tst_facility       .
*          ENDIF.
*
*        ENDIF.
        ls_load_plan-final_tst_facility   = ls_lotinfo-final_tst_facility .
        ls_load_plan-final_tst_st_dt          =  lv_start_date .
        lv_start_date                        =  lv_start_date  +  7 .
        ls_load_plan-final_exp_fin_dt        =  lv_start_date  .
        lv_start_date                       =  lv_start_date + 2 .

        READ TABLE  lt_mastdata INTO ls_mastdata  WITH KEY lot_no  = ls_lotinfo-lot_no
                                                         mtart   = 'ZTST'  .
        IF sy-subrc EQ 0.
          ls_load_plan-ft_plnnr  = ls_mastdata-route_number .
          ls_load_plan-ft_plnal  = ls_mastdata-route_counter .
          ls_load_plan-ft_plnnr_alt   = ls_mastdata-old_route .
          CLEAR ls_mastdata .
        ENDIF.
        " when final test  is empty   and finish facility is avaibale thean final testa nd finish facility are same
      ELSEIF ls_lotinfo-final_tst_facility   IS  INITIAL  AND  ls_lotinfo-finish_facility   IS NOT INITIAL   AND  ls_lotinfo-asy_facility   IS NOT INITIAL .
        ls_lotinfo-final_tst_facility = ls_lotinfo-finish_facility .
*        SHIFT ls_lotinfo-final_tst_facility  LEFT DELETING LEADING '0'  .
*        CLEAR lv_inpnr .
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = ls_lotinfo-final_tst_facility
*          IMPORTING
*            output = lv_inpnr.
*        READ TABLE  lt_crossref INTO ls_cross_ref   WITH KEY   sursys  =  '5'
*                                                              flag    = '6'
*                                                              inpnr   = lv_inpnr .
*        IF sy-subrc EQ 0.
*          ls_load_plan-final_tst_facility          =  ls_cross_ref-expnr             .
*          CLEAR  ls_cross_ref .
*        ELSE.
*          READ TABLE  lt_crossref INTO  ls_cross_ref   WITH KEY   sursys  =  '5'
*                                                      flag    = 'I'
*                                                      inpnr   = lv_inpnr .
*          IF sy-subrc EQ 0.
*            ls_load_plan-final_tst_facility           =  ls_cross_ref-expnr             .
*            CLEAR  ls_cross_ref .
*          ELSE.
*            ls_load_plan-final_tst_facility      =  ls_lotinfo-final_tst_facility       .
*          ENDIF.
*
*        ENDIF.
         ls_load_plan-final_tst_facility      = ls_lotinfo-final_tst_facility .
        ls_load_plan-final_tst_st_dt          = lv_start_date .
        lv_start_date                         = lv_start_date  +  7 .
        ls_load_plan-final_exp_fin_dt         = lv_start_date  .
        lv_start_date                         = lv_start_date + 2 .

        READ TABLE  lt_mastdata INTO ls_mastdata  WITH KEY lot_no  = ls_lotinfo-lot_no
                                                         mtart   = 'FERT'  .
        IF sy-subrc EQ 0.
          ls_load_plan-ft_plnnr  = ls_mastdata-route_number .
          ls_load_plan-ft_plnal  = ls_mastdata-route_counter .
          ls_load_plan-ft_plnnr_alt   = ls_mastdata-old_route .
          CLEAR ls_mastdata .
        ENDIF.

      ENDIF.
      " finish
      IF ls_lotinfo-finish_facility   IS NOT INITIAL .
*        SHIFT ls_lotinfo-finish_facility  LEFT DELETING LEADING '0'  .
*        CLEAR lv_inpnr .
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = ls_lotinfo-finish_facility
*          IMPORTING
*            output = lv_inpnr.
*        READ TABLE  lt_crossref INTO  ls_cross_ref    WITH KEY   sursys  =  '5'
*                                                              flag    = '6'
*                                                              inpnr   =   lv_inpnr  .
*        IF sy-subrc EQ 0.
*          ls_load_plan-finish_facility         =  ls_cross_ref-expnr             .
*          CLEAR  ls_cross_ref .
*        ELSE.
*          READ TABLE  lt_crossref INTO ls_cross_ref   WITH KEY   sursys  =  '5'
*                                                      flag    = 'I'
*                                                      inpnr   =    lv_inpnr  .
*          IF sy-subrc EQ 0.
*            ls_load_plan-finish_facility        =  ls_cross_ref-expnr             .
*            CLEAR  ls_cross_ref .
*          ELSE.
*            ls_load_plan-finish_facility         =  ls_lotinfo-finish_facility  .
*          ENDIF.
*
*        ENDIF.

        ls_load_plan-finish_facility   = ls_lotinfo-finish_facility .
        ls_load_plan-finish_st_dt           =  lv_start_date .
        lv_start_date                       =  lv_start_date  +  7  .
        ls_load_plan-finish_exp_fin_dt      =  lv_start_date  .
        lv_start_date                       =  lv_start_date + 2 .
        READ TABLE  lt_mastdata INTO ls_mastdata  WITH KEY lot_no  = ls_lotinfo-lot_no
                                                                mtart   = 'FERT'  .
        IF sy-subrc EQ 0.
          ls_load_plan-fin_plnnr  = ls_mastdata-route_number .
          ls_load_plan-fin_plnal  = ls_mastdata-route_counter .
          ls_load_plan-fin_plnnr_alt   = ls_mastdata-old_route .
          CLEAR ls_mastdata .
        ENDIF.
      ENDIF.


      " die coat
      IF ls_lotinfo-die_coat   IS NOT INITIAL .
        SHIFT ls_lotinfo-die_coat LEFT DELETING LEADING '0'  .
        CLEAR lv_inpnr .
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_lotinfo-die_coat
          IMPORTING
            output = lv_inpnr.
        READ TABLE  lt_crossref INTO data(ls_cross_ref)   WITH KEY   sursys  = '5'
                                                                       flag  = 'D'
                                                                      inpnr  = lv_inpnr  .
        IF sy-subrc EQ 0.
          ls_load_plan-dc_facility         =  ls_cross_ref-expnr             .
          CLEAR  ls_cross_ref .
        ELSE.

          ls_load_plan-dc_facility             =  ls_lotinfo-die_coat  .
        ENDIF.
      ENDIF.

      CHECK NOT is_ebrheader-req_no  IS INITIAL.
      lv_name = sy-mandt && is_ebrheader-req_no .

      ls_load_plan-db_instr =  read_text(                              " assembly instr
          EXPORTING
            id        = zcl_zebr_form_mpc_ext=>gc_tdid_die_instr    " Text ID
            name      = lv_name    " Name
            object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

      DATA(lv_wafer_no)   = ls_lotinfo-wafer_no  .
      REPLACE ALL OCCURRENCES OF  '"'   IN  lv_wafer_no WITH space .

      REPLACE all  OCCURRENCES OF ','  in lv_wafer_no  with '-'  .

      CONDENSE lv_wafer_no .

      IF ls_load_plan-db_instr CS '//'   .
        ls_load_plan-db_instr   =  lv_wafer_no    && ls_load_plan-db_instr+sy-fdpos .
      ELSE .
        ls_load_plan-db_instr   =  lv_wafer_no    &&  '//'   && ' ' && ls_load_plan-db_instr .
      ENDIF.

      ls_load_plan-sort_instr =  read_text(                              " assembly instr
      EXPORTING
        id        = zcl_zebr_form_mpc_ext=>gc_tdid_srt_instr    " Text ID
        name      = lv_name    " Name
        object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

      ls_load_plan-splins_text =  read_text(                              " assembly instr
          EXPORTING
            id        = zcl_zebr_form_mpc_ext=>gc_tdid_assy_instr    " Text ID
            name      = lv_name    " Name
            object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

      ls_load_plan-splins_text2 =  read_text(
            EXPORTING
              id        = zcl_zebr_form_mpc_ext=>gc_tdid_bump_instr    " Text ID
              name      = lv_name    " Name
              object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object
      ls_load_plan-splins_text4 =  read_text(
         EXPORTING
           id        = zcl_zebr_form_mpc_ext=>gc_tdid_test_instr    " Text ID
           name      = lv_name    " Name
           object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

      ls_load_plan-splins_text3 =  read_text(
          EXPORTING
            id        = zcl_zebr_form_mpc_ext=>gc_tdid_pbs_instr    " Text ID
            name      = lv_name    " Name
            object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

      ls_load_plan-splins_text5 =  read_text(
        EXPORTING
          id        = zcl_zebr_form_mpc_ext=>gc_tdid_fert_instr    " Text ID
          name      = lv_name    " Name
          object    = zcl_zebr_form_mpc_ext=>gc_object ).   " Texts: application object

      APPEND INITIAL LINE TO lt_ebrlot  ASSIGNING FIELD-SYMBOL(<fs_ebrlot>)  .
      MOVE-CORRESPONDING  ls_lotinfo  TO  <fs_ebrlot>   .
      <fs_ebrlot>-planning_id  = lv_planning_id .


      APPEND  ls_load_plan  TO lt_load_plan .
      CLEAR : ls_load_plan-licha           ,
              ls_load_plan-qty_primary_in  ,
              ls_load_plan-qty_primary_op  ,
              ls_load_plan-primary_uom_op   ,
              ls_load_plan-output_material  ,
              ls_load_plan-output_material ,
              ls_load_plan-input_material  ,
              ls_load_plan-dc_facility    ,
              ls_load_plan-final_tst_facility,
              ls_load_plan-pbs_facility,
              ls_load_plan-finish_facility,
              ls_load_plan-asy_facility ,
              ls_load_plan-ft_plnnr,
              ls_load_plan-ft_plnal,
              ls_load_plan-bump_facility  ,
              ls_load_plan-bump_start_dt  ,
              ls_load_plan-bump_fin_dt   ,
              lv_wafer_no.


    ENDLOOP  .

    IF lt_load_plan IS NOT INITIAL .
      MODIFY zscm_load_plan FROM TABLE lt_load_plan .
      IF sy-subrc EQ 0.
        COMMIT WORK .
      ENDIF.
    ENDIF.
    IF lt_ebrlot IS NOT INITIAL .
      MODIFY  zebr_t_lotinfo  FROM TABLE  lt_ebrlot .
      IF sy-subrc EQ 0.
        COMMIT WORK .
      ENDIF.
    ENDIF.
    IF lv_planning_id IS NOT INITIAL .      " erro qty is not aviable
      " send mail
      TRY.
          CALL METHOD zcl_ebr_utility=>email_functionality
            EXPORTING
              im_ebr        = is_ebrheader-req_no
              im_req_type   = 'N'
              im_req_status = is_ebrheader-req_status_no
              im_group      = zcl_ebr_utility=>zemail_grp_sub_lp
*            IMPORTING
*             et_notif      =
            .
        CATCH cx_abap_invalid_value .
      ENDTRY.

      IF lo_exception IS BOUND.
        lo_exception->get_msg_container( )->add_message(
              EXPORTING
               iv_msg_type    = 'S'
               iv_msg_id      = '00'
               iv_msg_number  = '123'
               iv_msg_text    = 'Planning Id Successfully created '  &&  lv_planning_id ).
        RAISE EXCEPTION lo_exception.
        RETURN.
      ENDIF.

    ELSE.
      IF lo_exception IS BOUND.
        lo_exception->get_msg_container( )->add_message(
              EXPORTING
               iv_msg_type     = 'E'
               iv_msg_id      = '00'
               iv_msg_number  = '123'
               iv_msg_text    =   'Planning ID is not generated ' ).
        RAISE EXCEPTION lo_exception.
        RETURN.
      ENDIF.
    ENDIF.
    CLEAR  : ls_out_mara ,
             lv_input_mtart.


  ENDMETHOD.


  METHOD VALIDATE_EBR.
*   Hybrid case Lot number's Identifier should be same
*   Valid Lot
*   Lot should be present in zzdb_receipt - Lot is not inducted
*•  Sort Material not found for given ZFAB error out
*•  Check if lot is blocked in MCHB - CHPEM
*•  Input Lot and Output material should be connected in BOM
*•  GTS Classification? For which parts?
*•  Die Coat required as per BOM but lot is already Die Coated at what level what are the checks on DC?
*•  Validations around DC Facility ?
*•  Lot not inducted for LICHA?
*•  Input lot should always be ZSRT for Phase 1, Output material is all , This validation should be done in Submit NPP Screen before Load plan submission
    TYPES  : BEGIN OF ty_lot  ,
               licha TYPE lichn,
             END OF ty_lot .

    DATA  :lv_identifer_first TYPE string,
           lt_stpo_bom        TYPE ztt_bom_com_level,
           lt_allocvaluesnum  TYPE TABLE OF  bapi1003_alloc_values_num,
           lt_allocvalueschar TYPE  TABLE OF bapi1003_alloc_values_char,
           lt_allocvaluescurr TYPE  TABLE OF bapi1003_alloc_values_curr,
           lt_startlot        TYPE  TABLE OF  ty_lot,
           lo_exception       TYPE REF TO /iwbep/cx_mgw_tech_exception,
           lt_return          TYPE bapiret2_t,
           lv_clabs           TYPE  int4,
           lv_date            TYPE rc29l-datub,
           lv_datuv           TYPE rc29l-datuv,
           lt_stpov           TYPE STANDARD TABLE OF stpov,
           lt_equicat         TYPE TABLE OF cscequi,
           lt_kndcat          TYPE TABLE OF cscknd,
           lt_matcat          TYPE TABLE OF cscmat,
           lt_stdcat          TYPE TABLE OF cscstd,
           lt_tplcat          TYPE TABLE OF csctpl,
           lt_mast            TYPE STANDARD TABLE OF zebr_t_mast_data,
           lt_prjcat          TYPE TABLE OF cscprj,
           lv_mtart           TYPE  mara-mtart,
           lrt_mtart          TYPE RANGE OF  mtart,
           lv_lines           TYPE  i,
           lrt_licha          TYPE RANGE OF lichn.


    APPEND INITIAL LINE TO  lrt_mtart ASSIGNING FIELD-SYMBOL(<fr_mtart>).
    <fr_mtart>-sign   = 'I' .
    <fr_mtart>-option = 'EQ'  .
    <fr_mtart>-low    =  'ZFAB'  .


    APPEND INITIAL LINE TO  lrt_mtart ASSIGNING <fr_mtart>.
    <fr_mtart>-sign   = 'I' .
    <fr_mtart>-option = 'EQ'  .
    <fr_mtart>-low    =  'ZSRT'  .


    CREATE OBJECT lo_exception.

    IF is_ebrheader-masterdata_status  =  'R'  OR  is_ebrheader-masterdata_status  =  ' '  .
      IF lo_exception IS BOUND.
        lo_exception->get_msg_container( )->add_message(
              EXPORTING
               iv_msg_type     = 'E'
               iv_msg_id      = '00'
               iv_msg_number  = '123'
               iv_msg_text    = 'Master Data is not ready  to Submiting  to  Load Plan' ).
        RAISE EXCEPTION lo_exception.
        RETURN.
      ENDIF.

    ENDIF.
    IF   is_ebrheader-supply_status  = 'R'  OR  is_ebrheader-supply_status  IS INITIAL  .
      IF lo_exception IS BOUND.
        lo_exception->get_msg_container( )->add_message(
              EXPORTING
               iv_msg_type     = 'E'
               iv_msg_id      = '00'
               iv_msg_number  = '123'
               iv_msg_text    = 'Supply is not ready  for Submiting  to  Load Plan' ).
        RAISE EXCEPTION lo_exception.
        RETURN.
      ENDIF.

    ENDIF.

    IF is_ebrheader-relid IS NOT INITIAL .
      SELECT SINGLE  external_id    FROM /rpm/item_d   INTO @DATA(lv_projectid)  WHERE external_id  =  @is_ebrheader-relid
                                                                                  AND  item_type    LIKE  'ZRIM%' . "#EC CI_NOFIELD
      IF sy-subrc  NE  0.
        IF lo_exception IS BOUND.
          lo_exception->get_msg_container( )->add_message(
                EXPORTING
                 iv_msg_type     = 'E'
                 iv_msg_id      = '00'
                 iv_msg_number  = '123'
                 iv_msg_text    = 'Enter a Valid Rel ID' ).
          RAISE EXCEPTION lo_exception.
          RETURN.
        ENDIF.

      ENDIF.
    ENDIF.

    IF is_ebrheader-nprf_no IS NOT INITIAL .

      IF ( is_ebrheader-nprf_no+0(1)   = 'P' or is_ebrheader-nprf_no+0(1)   = 'p' )  AND strlen( is_ebrheader-nprf_no )  = '7' .
      ELSE  .
        IF lo_exception IS BOUND.
          lo_exception->get_msg_container( )->add_message(
                EXPORTING
                 iv_msg_type     = 'E'
                 iv_msg_id      = '00'
                 iv_msg_number  = '123'
                 iv_msg_text    = 'Invalid NPRF No (NPRF No. should start with "P" and should be of 7 characters)' ).
          RAISE EXCEPTION lo_exception.
          RETURN.
        ENDIF.

      ENDIF.
    ENDIF.

*    SELECT a~matnr , a~charg  , a~licha , b~clabs ,b~cspem , c~mtart ,c~mfrpn
*                           FROM  mch1  AS a INNER JOIN  mchb  AS b  ON a~matnr  = b~matnr     AND a~charg   =  b~charg
*                                                                        INNER JOIN  mara AS c ON b~matnr  =  c~matnr
*                                                                      INTO TABLE @DATA(lt_mch1)  FOR ALL ENTRIES IN  @is_ebrheader-ebrheadlotnav
*                                                                      WHERE a~licha EQ @is_ebrheader-ebrheadlotnav-lot_no
*                                                                       AND b~clabs GT 0.
    SELECT a~matnr , a~mtart, a~groes , a~mfrpn , b~charg , b~licha  ,c~werks , c~lgort   , c~clabs  ,c~cspem,c~ceinm
     INTO TABLE  @DATA(lt_mch1) FROM  mara  AS a  INNER JOIN mch1  AS b
                                          ON  a~matnr   = b~matnr
                                          INNER JOIN mchb AS c
                                          ON b~matnr  = c~matnr
                                         AND b~charg  = c~charg
                                         INNER JOIN t320 AS d
                                          ON    c~werks  =  d~werks
                                          AND   c~lgort  =  d~lgort
                           FOR ALL ENTRIES IN  @is_ebrheader-ebrheadlotnav
                                        WHERE b~licha EQ @is_ebrheader-ebrheadlotnav-lot_no
                                          AND c~clabs GT 0.
    IF sy-subrc  EQ  0 .
      SORT lt_mch1  BY matnr  .
      " error out when  lot  materials are not sort
      DATA(lt_mch1_zsrt)  = lt_mch1 .
      LOOP AT  is_ebrheader-ebrheadlotnav  INTO  DATA(ls_headlot).
        IF ls_headlot-output_die_type  EQ  'NEW' .
          IF lo_exception IS BOUND.
            lo_exception->get_msg_container( )->add_message(
                  EXPORTING
                   iv_msg_type     = 'E'
                   iv_msg_id      = '00'
                   iv_msg_number  = '123'
                   iv_msg_text    = 'Output Dietype is is NEW' && ' '  && ls_headlot-output_part ) .
            RAISE EXCEPTION lo_exception.
            RETURN.
          ENDIF.

        ELSE.
          READ TABLE lt_mch1 INTO DATA(ls_mch1)  WITH KEY   licha  = ls_headlot-lot_no.
          IF sy-subrc EQ 0.
            CLEAR  lv_clabs  .
            IF lv_mtart IS INITIAL  .
              lv_mtart  =  ls_mch1-mtart .
*              " owner code
              CALL METHOD zcl_zebr_form_dpc_ext=>get_batch_char
                EXPORTING
                  iv_matnr           = ls_mch1-matnr
                  iv_charg           = ls_mch1-charg
                IMPORTING
                  et_allocvaluesnum  = DATA(lt_allocvaluesnum_oc)
                  et_allocvalueschar = DATA(lt_allocvalueschar_oc)
                  et_allocvaluescurr = DATA(lt_allocvaluescurr_oc)
                  et_return          = DATA(lt_return_oc).
              READ TABLE lt_allocvalueschar_oc INTO DATA(ls_char) WITH KEY  charact  = 'OWNER_CODE' .
              IF sy-subrc EQ 0.
                DATA(lv_owner_code)  = ls_char-value_neutral  .
                CLEAR ls_char .
              ENDIF.
*              IF lv_owner_code EQ  'X' AND is_ebrheader-pcn_type  IS INITIAL .
*                IF lo_exception IS BOUND.
*                  lo_exception->get_msg_container( )->add_message(
*                        EXPORTING
*                         iv_msg_type     = 'E'
*                         iv_msg_id      = '00'
*                         iv_msg_number  = '123'
*                         iv_msg_text    = 'PCN type is Mandatory :  Lot'  && ls_headlot-lot_no && 'has Owner code X' ).
*                  RAISE EXCEPTION lo_exception.
*                  RETURN.
*                ENDIF.
*
*
*              ENDIF.

              CLEAR :  lv_owner_code .
              REFRESH  : lt_allocvaluesnum_oc[] ,
                         lt_allocvalueschar_oc[] ,
                         lt_allocvaluescurr_oc[] ,
                         lt_return_oc[].
            ELSEIF lv_mtart  NE ls_mch1-mtart .
              IF lo_exception IS BOUND.
                lo_exception->get_msg_container( )->add_message(
                      EXPORTING
                       iv_msg_type     = 'E'
                       iv_msg_id      = '00'
                       iv_msg_number  = '123'
                       iv_msg_text    = 'All  lots are not of same material Type'  && ' '  && lv_mtart ).
                RAISE EXCEPTION lo_exception.
                RETURN.
              ENDIF.

            ENDIF.
            lv_clabs   =  ls_mch1-clabs   .
            "  if lot' are fab   divide lot start qty  by gdpw   to get wafer qty
            IF lv_mtart  = 'ZFAB'  .
              DATA(lv_lot_start_qty)  = ls_headlot-lot_start_qty  /  is_ebrheader-gdpw .
            ELSE.
              lv_lot_start_qty  =   ls_headlot-lot_start_qty .
            ENDIF.
            IF  lv_lot_start_qty GT  lv_clabs   .
              " error qty is not available
              IF lo_exception IS BOUND.
                lo_exception->get_msg_container( )->add_message(
                      EXPORTING
                       iv_msg_type     = 'E'
                       iv_msg_id      = '00'
                       iv_msg_number  = '123'
                       iv_msg_text    = ls_headlot-lot_no &&  'Does not have enough quantity' ).
                RAISE EXCEPTION lo_exception.
                RETURN.
              ENDIF.
            ENDIF.
          ELSE.
            IF lo_exception IS BOUND.
              lo_exception->get_msg_container( )->add_message(
                    EXPORTING
                     iv_msg_type     = 'E'
                     iv_msg_id      = '00'
                     iv_msg_number  = '123'
                     iv_msg_text    = ls_headlot-lot_no &&  'Does not have enough quantity/ Quantity is in interim Location' ).
              RAISE EXCEPTION lo_exception.
              RETURN.
            ENDIF.
          ENDIF.
          CLEAR ls_mch1 .
        ENDIF.
        CLEAR lv_lot_start_qty .
      ENDLOOP.
      " facilities will be same for all records in lotinfo table so reading using index 1
      DATA(lt_lotinfo)  =  is_ebrheader-ebrheadlotnav  .
      READ TABLE  lt_lotinfo INTO DATA(ls_lotinfo_fac)   INDEX 1  .
      IF sy-subrc EQ 0.
        IF ls_lotinfo_fac-asy_facility  EQ 'ANY'   .
          " error  not part of output material
          IF lo_exception IS BOUND.
            lo_exception->get_msg_container( )->add_message(
            EXPORTING
              iv_msg_type     = 'E'
              iv_msg_id      = '00'
              iv_msg_number  = '123'
              iv_msg_text    =  'Select Assembly Facilty Other "ANY" ' ).
            RAISE EXCEPTION lo_exception.
            RETURN.
          ENDIF.
        ENDIF.

        IF ls_lotinfo_fac-bump_facility  EQ  'ANY'  .
          " error  not part of output material
          IF lo_exception IS BOUND.
            lo_exception->get_msg_container( )->add_message(
            EXPORTING
              iv_msg_type     = 'E'
              iv_msg_id      = '00'
              iv_msg_number  = '123'
              iv_msg_text    =  'Select BUMP Facilty Other "ANY" ' ).
            RAISE EXCEPTION lo_exception.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.
      SORT lt_lotinfo  BY output_part .
      LOOP AT lt_lotinfo  INTO DATA(ls_lotinfo).
        AT NEW  output_part  .
          CALL METHOD zcl_bom_explode_util=>bom_explode
            EXPORTING
              iv_matnr              = ls_lotinfo-output_part
              iv_werks              = '4900'
*             ls_stpo               =
              iv_get_marterial_type = 'X'
*              iv_usage_probability  = 'X'
            CHANGING
              it_stpo               = lt_stpo_bom.
          " get output material  type
          SELECT  SINGLE matnr , mtart,  mfrpn INTO  @DATA(ls_out_mara) FROM mara  WHERE matnr  = @ls_lotinfo-output_part .
          DATA(lt_stpo_mat)  = lt_stpo_bom .
          DELETE lt_stpo_mat  WHERE  mtart NOT IN lrt_mtart .

          " get die coat vendors
          IF lt_stpo_bom IS NOT INITIAL  .
            " get lot details
            CLEAR lrt_licha[].
            LOOP AT  lt_lotinfo  INTO DATA(ls_lotinfo_dc)  WHERE output_part = ls_lotinfo-output_part .

              APPEND INITIAL LINE TO lrt_licha ASSIGNING FIELD-SYMBOL(<fr_licha>).
              <fr_licha>-sign   = 'I' .
              <fr_licha>-option = 'EQ' .
              <fr_licha>-low    =  ls_lotinfo_dc-lot_no  .
            ENDLOOP.
            IF lt_stpo_mat IS NOT INITIAL .
              SELECT a~matnr , a~mtart, a~groes , a~mfrpn , b~charg , b~licha  ,c~werks , c~lgort   , c~clabs  ,c~ceinm
                     INTO TABLE  @DATA(lt_mara_mch1) FROM  mara  AS a  INNER JOIN mch1  AS b
                                                          ON  a~matnr   = b~matnr
                                                          INNER JOIN mchb AS c
                                                          ON b~matnr  = c~matnr
                                                         AND b~charg  = c~charg
                                                         INNER JOIN t320 AS d
                                                          ON    c~werks  =  d~werks
                                                          AND   c~lgort  =  d~lgort
                                             FOR ALL ENTRIES IN @lt_stpo_mat
                                                          WHERE a~matnr  EQ @lt_stpo_mat-matnr
                                                           AND  a~mtart  IN @lrt_mtart
                                                           AND  b~licha  IN @lrt_licha
                                                           AND ( c~clabs    GT  0 OR c~ceinm GT  0 ).
            ENDIF.
          ENDIF.
        ENDAT .
        READ TABLE lt_mch1 INTO DATA(ls_mch1_valid)  WITH KEY licha  =  ls_lotinfo-lot_no .
        IF sy-subrc EQ 0.
          IF ls_out_mara-mtart  = 'ZSRT'  AND ls_mch1_valid-mtart  = 'ZSRT'  .

            " error  not part of output material
            IF lo_exception IS BOUND.
              lo_exception->get_msg_container( )->add_message(
              EXPORTING
                iv_msg_type     = 'E'
                iv_msg_id      = '00'
                iv_msg_number  = '123'
                iv_msg_text    =  'Both '  && ls_lotinfo-lot_no   && ' and '   && ls_out_mara-mfrpn && ' Are  Sort Materials' ).
              RAISE EXCEPTION lo_exception.
              RETURN.
            ENDIF.

          ENDIF.
          READ TABLE  lt_stpo_bom  INTO   DATA(ls_stpo_bom)  WITH KEY  matnr  =   ls_mch1_valid-matnr .
          IF sy-subrc NE 0 .
            " error  not part of output material
            IF lo_exception IS BOUND.
              lo_exception->get_msg_container( )->add_message(
              EXPORTING
                iv_msg_type     = 'E'
                iv_msg_id      = '00'
                iv_msg_number  = '123'
                iv_msg_text    = ls_lotinfo-lot_no  &&  'is not part of '   &&  ls_out_mara-mfrpn ).
              RAISE EXCEPTION lo_exception.
              RETURN.
            ENDIF.
          ENDIF.
        ENDIF.
        " assumbing vendor lot with quantity will be avaialble only  either one of  zfab or zsrt
        READ TABLE  lt_mara_mch1 INTO DATA(ls_mara_mch1)  WITH KEY licha  = ls_lotinfo-lot_no.
        IF sy-subrc EQ 0.
          IF ls_mara_mch1-mtart  = 'ZSRT'  .
            READ TABLE lt_stpo_bom INTO DATA(ls_stpo)  WITH KEY matnr  = ls_mara_mch1-matnr .
            IF sy-subrc EQ 0.
              DATA(lv_sortf)   =  ls_stpo-sortf .
            ENDIF.
          ELSEIF ls_mara_mch1-mtart  = 'ZFAB'.
            lv_date  = sy-datum .
            lv_datuv = sy-datum .
            CALL FUNCTION 'CS_WHERE_USED_MAT'
              EXPORTING
                datub                      = lv_date
                datuv                      = lv_datuv
                matnr                      = ls_mara_mch1-matnr
                stlan                      = '1'
                stltp                      = 'M'
              TABLES
                wultb                      = lt_stpov
                equicat                    = lt_equicat
                kndcat                     = lt_kndcat
                matcat                     = lt_matcat
                stdcat                     = lt_stdcat
                tplcat                     = lt_tplcat
                prjcat                     = lt_prjcat
              EXCEPTIONS
*               CALL_INVALID               = 01
                material_not_found         = 02
                no_where_used_rec_found    = 03
                no_where_used_rec_selected = 04
                no_where_used_rec_valid    = 05.
            IF sy-subrc NE 0 .
              REFRESH : lt_equicat[],
                   lt_kndcat[],
                   lt_matcat[],
                   lt_stdcat[],
                   lt_tplcat[],
                   lt_prjcat[].
              EXIT.
            ELSE.
              REFRESH : lt_equicat[],
                        lt_kndcat[],
                        lt_matcat[],
                        lt_stdcat[],
                        lt_tplcat[],
                        lt_prjcat[].
              DELETE lt_stpov WHERE bmtyp NE 'M' .
              SORT lt_stpov BY matnr bmtyp ttidx .
              DELETE ADJACENT DUPLICATES FROM lt_stpov COMPARING matnr bmtyp ttidx idnrk .
              " fab to sort will be always one is  one
              READ TABLE lt_stpov INTO DATA(ls_stpov)   INDEX 1 .
              IF sy-subrc EQ 0.
                CLEAR ls_stpo.
                READ TABLE lt_stpo_bom INTO ls_stpo  WITH KEY matnr  = ls_stpov-matnr .
                IF sy-subrc EQ 0.
                  CLEAR lv_sortf .
                  lv_sortf   =  ls_stpo-sortf .
                ENDIF.
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.
        " die coat facility validation  check if die coat is empty whethere thier is any valid  Die coat in BOM
        IF ls_lotinfo-die_coat  IS INITIAL .
          " check whether that lot material has any  die coat vendor
          SELECT a~matnr , a~matkl , b~werks , b~lifnr   INTO TABLE @DATA(lt_diecoat)  FROM mara AS a INNER JOIN eord AS b "#EC CI_NO_TRANSFORM
                                                             ON a~matnr   = b~matnr
                                                          FOR ALL ENTRIES IN @lt_stpo_bom
                                                         WHERE a~matnr = @lt_stpo_bom-matnr
                                                          AND  a~matkl  =  '119'  .
          IF sy-subrc EQ 0.
            SELECT  lifnr , name1  FROM lfa1  INTO  TABLE @DATA(lt_lfa1) "#EC CI_NO_TRANSFORM
              FOR ALL ENTRIES IN  @lt_diecoat  WHERE lifnr   =  @lt_diecoat-lifnr .
            IF sy-subrc  EQ 0 .
*          SORT lt_diecoat  BY lifnr  .
*          DELETE ADJACENT DUPLICATES FROM  lt_diecoat COMPARING lifnr  .
              LOOP AT lt_diecoat  INTO DATA(ls_diecoat)   .
                READ TABLE  lt_stpo_bom INTO ls_stpo_bom  WITH KEY matnr  = ls_diecoat-matnr .
                IF sy-subrc EQ 0    AND ( ( ls_stpo_bom-sortf  = lv_sortf )  OR ls_stpo_bom-sortf IS INITIAL ).
                  " error
                  IF lo_exception IS BOUND.
                    lo_exception->get_msg_container( )->add_message(
                     EXPORTING
                       iv_msg_type     = 'E'
                       iv_msg_id      = '00'
                       iv_msg_number  = '123'
                       iv_msg_text    =  'Enter  die Coat vendor for lot' && ls_lotinfo-lot_no  ).
                    RAISE EXCEPTION lo_exception.
                    RETURN.
                  ENDIF.


                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.



        ENDIF.
        IF ls_lotinfo-fab_out_date  IS INITIAL .
          " error
          IF lo_exception IS BOUND.
            lo_exception->get_msg_container( )->add_message(
             EXPORTING
               iv_msg_type     = 'E'
               iv_msg_id      = '00'
               iv_msg_number  = '123'
               iv_msg_text    =  'Enter  Fab Out date for lot' && ls_lotinfo-lot_no  ).
            RAISE EXCEPTION lo_exception.
            RETURN.
          ENDIF.

        ENDIF.
      ENDLOOP.

*
*      " check whetehr all lot  are available in  zzdb_receipt
*      LOOP  AT lt_mch1 INTO DATA(ls_mch1_zzdd)   .
*        " using select single  because statring lot and   licha are not of same length and expecting less
*        SELECT SINGLE startinglot  FROM zzdb_receipt  INTO  @DATA(ls_dbreceipt)   WHERE startinglot  =  @ls_mch1_zzdd-licha  . "#EC CI_NOFIELD
*        IF sy-subrc EQ  0 .                                                                             "  lot is not inducted
*          " error
*          IF lo_exception IS BOUND.
*            lo_exception->get_msg_container( )->add_message(
*             EXPORTING
*               iv_msg_type     = 'E'
*               iv_msg_id      = '00'
*               iv_msg_number  = '123'
*               iv_msg_text    =  'Lot ' && ls_mch1_zzdd-licha  &&  ' is not part of zzdb_receipt(lot not yet inducted)'  ).
*            RAISE EXCEPTION lo_exception.
*            RETURN.
*          ENDIF.
*
*        ENDIF.
*        CLEAR : ls_mch1_zzdd .
*      ENDLOOP .
    ELSE.
      " error qty is not available
      IF lo_exception IS BOUND.
        lo_exception->get_msg_container( )->add_message(
              EXPORTING
               iv_msg_type     = 'E'
               iv_msg_id      = '00'
               iv_msg_number  = '123'
               iv_msg_text    = 'Lots Does not have enough quantity' ).
        RAISE EXCEPTION lo_exception.
        RETURN.
      ENDIF.
    ENDIF.



    " more than one lot mean hybrid case
    READ TABLE is_ebrheader-ebrheadlotnav   TRANSPORTING NO FIELDS   INDEX 2 .
    IF sy-subrc EQ 0.
      LOOP AT  is_ebrheader-ebrheadlotnav   ASSIGNING FIELD-SYMBOL(<fs_ebrlotinfo>)  .
*        IF lv_identifer_first    IS  INITIAL  .
        READ TABLE lt_mch1  INTO  DATA(ls_mch1_iden)  WITH KEY licha = <fs_ebrlotinfo>-lot_no   BINARY SEARCH .
        IF sy-subrc EQ  0.
          " batch CHARACTERSTICS
          CALL METHOD zcl_zebr_form_dpc_ext=>get_batch_char
            EXPORTING
              iv_matnr           = ls_mch1_iden-matnr
              iv_charg           = ls_mch1_iden-charg
            IMPORTING
              et_allocvaluesnum  = lt_allocvaluesnum
              et_allocvalueschar = lt_allocvalueschar
              et_allocvaluescurr = lt_allocvaluescurr
              et_return          = lt_return.
          .
          READ TABLE  lt_allocvalueschar INTO DATA(ls_batchchar)  WITH KEY charact = 'IDENTIFIER'  .
          IF sy-subrc EQ 0.
            IF lv_identifer_first  IS INITIAL .
              lv_identifer_first   =  ls_batchchar-value_char .
            ELSE.
              IF lv_identifer_first  NE   ls_batchchar-value_char .
                " error
                IF lo_exception IS BOUND.
                  lo_exception->get_msg_container( )->add_message(
                   EXPORTING
                     iv_msg_type    = 'E'
                     iv_msg_id      = '00'
                     iv_msg_number  = '123'
                     iv_msg_text    = 'Lot  identifier are not same'  ).
                  RAISE EXCEPTION lo_exception.
                  RETURN.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

        ELSE .

        ENDIF.

        REFRESH  :  lt_allocvaluesnum[] ,
                    lt_allocvalueschar[] ,
                    lt_allocvaluescurr[] ,
                    lt_return.

      ENDLOOP.
    ENDIF.

    " spare part indicator logic
    " checking spart indiactor logic is same as checking whetehr all sorts for that phantom is matched with all sort lots in hybrid scenario
    DATA(lt_lotinfo_hyb)   =  is_ebrheader-ebrheadlotnav[].
    SORT lt_lotinfo_hyb  BY output_part  .
    LOOP  AT    lt_lotinfo_hyb  INTO DATA(ls_lotinfo_hyb)  .
      DATA(ls_lotinfo_hyb_n)   =  ls_lotinfo_hyb  .   "  moving data since at new will have *
      AT NEW  output_part .
        CLEAR  : lv_lines ,
                 lt_mast[] .
        " in ebr mast data table we are updating  all sorts in hybrid case  comparing the count in zebt_t-mast-data and lotinfo table
        SELECT  * FROM zebr_t_mast_data  INTO TABLE  lt_mast  WHERE  req_no        = ls_lotinfo_hyb_n-req_no "#EC CI_ALL_FIELDS_NEEDED
                                                                AND  output_matnr  = ls_lotinfo_hyb_n-output_part
                                                                AND  mtart         = 'ZSRT'  .
        IF sy-subrc  EQ  0 .
          lv_lines   = lines( lt_mast ) .
        ENDIF.
        DATA(lt_lot_count)   =   lt_lotinfo_hyb .
        DELETE  lt_lot_count  WHERE output_part  NE  ls_lotinfo_hyb_n-output_part .
        IF lt_lot_count IS NOT INITIAL .
          DATA(lv_lot_count)  =  lines( lt_lot_count )  .
        ENDIF.
        " validate sort lot count should be same as  phantom  sorts counts
        IF lv_lines  NE  lv_lot_count   .
          " error
          " error
          IF lo_exception IS BOUND.
            lo_exception->get_msg_container( )->add_message(
             EXPORTING
               iv_msg_type    = 'E'
               iv_msg_id      = '00'
               iv_msg_number  = '123'
               iv_msg_text    = 'ALL Sort lots are not entered  in hybrid scenario'  ).
            RAISE EXCEPTION lo_exception.
            RETURN.
          ENDIF.

        ENDIF .
        CLEAR  : lv_lot_count .
        REFRESH : lt_lot_count[] .
      ENDAT .
      " hybrid case
      " validate whether all sorts of phanton avaiable or not   in lot master
      " validate sort lot count should be same as  phantom  sorts counts
      IF lv_lines  GT 1 .
        READ TABLE lt_mch1 INTO DATA(ls_mch1_hyb)  WITH KEY  licha  =  ls_lotinfo_hyb-lot_no .
        IF sy-subrc EQ 0.
          IF ls_mch1_hyb-mtart  EQ 'ZSRT' .
            READ TABLE  lt_mast  INTO DATA(ls_mast)  WITH KEY   output_matnr  =  ls_lotinfo_hyb-output_part
                                                                matnr         =  ls_mch1_hyb-matnr  .
            IF sy-subrc NE   0  .
              "  error
              IF lo_exception IS BOUND.
                lo_exception->get_msg_container( )->add_message(
                 EXPORTING
                   iv_msg_type    = 'E'
                   iv_msg_id      = '00'
                   iv_msg_number  = '123'
                   iv_msg_text    = 'Lot'  && ls_mch1_hyb-licha && 'is  not part of bom' ).
                RAISE EXCEPTION lo_exception.
                RETURN.
              ENDIF.

            ELSEIF   ls_mch1_hyb-mtart  EQ 'ZFAB' .
              " getsort materil  for that fab  and check whether is sort is part of  master  or not .
              " get bom higher level material
              lv_date  = sy-datum .
              lv_datuv = sy-datum .
              CALL FUNCTION 'CS_WHERE_USED_MAT'
                EXPORTING
                  datub                      = lv_date
                  datuv                      = lv_datuv
                  matnr                      = ls_mch1_hyb-matnr
                  stlan                      = '1'
                  stltp                      = 'M'
                TABLES
                  wultb                      = lt_stpov
                  equicat                    = lt_equicat
                  kndcat                     = lt_kndcat
                  matcat                     = lt_matcat
                  stdcat                     = lt_stdcat
                  tplcat                     = lt_tplcat
                  prjcat                     = lt_prjcat
                EXCEPTIONS
*                 CALL_INVALID               = 01
                  material_not_found         = 02
                  no_where_used_rec_found    = 03
                  no_where_used_rec_selected = 04
                  no_where_used_rec_valid    = 05.
              IF sy-subrc NE 0 .
                REFRESH : lt_equicat[],
                     lt_kndcat[],
                     lt_matcat[],
                     lt_stdcat[],
                     lt_tplcat[],
                     lt_prjcat[].
                EXIT.
              ELSE.
                REFRESH : lt_equicat[],
                          lt_kndcat[],
                          lt_matcat[],
                          lt_stdcat[],
                          lt_tplcat[],
                          lt_prjcat[].
                DELETE lt_stpov WHERE bmtyp NE 'M' .
                SORT lt_stpov BY matnr bmtyp ttidx .
                DELETE ADJACENT DUPLICATES FROM lt_stpov COMPARING matnr bmtyp ttidx idnrk .
                LOOP AT lt_stpov INTO ls_stpov  .
                  " assuming only one sort will be agisne fab
                  READ TABLE  lt_mast  INTO DATA(ls_mast_hyb)  WITH KEY   output_matnr  =  ls_lotinfo_hyb-output_part
                                                               matnr        =  ls_stpov-matnr  .
                  IF sy-subrc NE   0  .
                    "  error
                    IF lo_exception IS BOUND.
                      lo_exception->get_msg_container( )->add_message(
                       EXPORTING
                         iv_msg_type    = 'E'
                         iv_msg_id      = '00'
                         iv_msg_number  = '123'
                         iv_msg_text    = 'Lot'  && ls_mch1_hyb-licha && 'is  not part of bom' ).
                      RAISE EXCEPTION lo_exception.
                      RETURN.
                    ENDIF.
                    CLEAR  ls_mast  .
                  ELSE.
                    EXIT .
                  ENDIF.
                  CLEAR : ls_stpov ,
                          ls_mast_hyb .
                ENDLOOP.
                REFRESH  lt_stpov  .
              ENDIF.
            ENDIF.

          ENDIF.
          CLEAR  : ls_mch1_hyb .
        ENDIF .
      ENDIF .
      CLEAR :ls_lotinfo_hyb_n ,ls_lotinfo_hyb .
    ENDLOOP.



  ENDMETHOD.


  METHOD VALIDATE_PLANNING_ID.
    data :  lo_exception       TYPE REF TO /iwbep/cx_mgw_tech_exception.
    SELECT  aufnr , planning_id  , objnr FROM aufk INTO TABLE @DATA(lt_aufk)  WHERE planning_id  = @iv_planning_id  .    "#EC CI_NO_TRANSFORM
    IF sy-subrc EQ 0.
      SELECT   * FROM jest  INTO TABLE @DATA(lt_jest)  for ALL ENTRIES IN  @lt_aufk                                      "#EC CI_NO_TRANSFORM
                                                      WHERE objnr   = @lt_aufk-objnr
                                                        AND  stat   =  'I0045'
                                                        AND  inact  =   ' '  .
      IF lt_jest IS NOT INITIAL .
        LOOP AT  lt_aufk INTO DATA(ls_aufk)   .
          READ TABLE  lt_jest  WITH KEY   objnr  =  ls_aufk-objnr  TRANSPORTING NO FIELDS.
          IF sy-subrc EQ 0.
            DELETE  lt_aufk   WHERE planning_id  = ls_aufk-planning_id .
          ENDIF .
        ENDLOOP.
      ENDIF.
      IF lt_aufk IS NOT INITIAL .
        DATA(lv_prod_ord)  = abap_true .
      ENDIF .
    ENDIF.

    SELECT ebeln  , ebelp   , afnam   , loekz FROM ekpo INTO TABLE @DATA(lt_ekpo)  WHERE afnam  =  @iv_planning_id
                                                                                     AND loekz  =  ' '  .
    IF sy-subrc EQ 0.
      DATA(lv_sto)   = abap_true  .
    ENDIF.

    IF lv_sto  EQ abap_true  or lv_prod_ord  EQ abap_true .
         CREATE OBJECT lo_exception.
      IF lo_exception IS BOUND.
          lo_exception->get_msg_container( )->add_message(
                EXPORTING
                 iv_msg_type     = 'E'
                 iv_msg_id      = '00'
                 iv_msg_number  = '123'
                 iv_msg_text    = 'Open Production Orders or Open STO’s are already created against this EBR Number ' ).
          RAISE EXCEPTION lo_exception.
          RETURN.
        ENDIF.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
