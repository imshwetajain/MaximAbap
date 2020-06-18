class ZCL_ZEBR_DASHBOARD_DPC_EXT_1 definition
  public
  inheriting from ZCL_ZEBR_DASHBOARD_DPC
  create public .

public section.
protected section.

  methods EBRBUSET_GET_ENTITY
    redefinition .
  methods EBRBUSET_GET_ENTITYSET
    redefinition .
  methods EBRCOUNTSET_GET_ENTITY
    redefinition .
  methods EBRCOUNTSET_GET_ENTITYSET
    redefinition .
  methods EBRCREATEBYSET_GET_ENTITYSET
    redefinition .
  methods EBRDATASET_GET_ENTITY
    redefinition .
  methods EBRDATASET_GET_ENTITYSET
    redefinition .
  methods EBRHELPLINKSET_GET_ENTITY
    redefinition .
  methods EBRHELPLINKSET_GET_ENTITYSET
    redefinition .
  methods EBRNOTIFSET_GET_ENTITY
    redefinition .
  methods EBRNOTIFSET_GET_ENTITYSET
    redefinition .
  methods EBRNPPPLANNERSET_GET_ENTITYSET
    redefinition .
  methods EBRNPPSET_GET_ENTITY
    redefinition .
  methods EBRNPPSET_GET_ENTITYSET
    redefinition .
  methods EBRPACKAGESET_GET_ENTITYSET
    redefinition .
  methods EBRREQUESTTYPESE_GET_ENTITYSET
    redefinition .
  methods EBRROLESSET_GET_ENTITY
    redefinition .
  methods EBRROLESSET_GET_ENTITYSET
    redefinition .
  methods EBRSTATUSSET_GET_ENTITYSET
    redefinition .
  methods EBRVARIANTSET_CREATE_ENTITY
    redefinition .
  methods EBRVARIANTSET_GET_ENTITY
    redefinition .
  methods EBRVARIANTSET_GET_ENTITYSET
    redefinition .
  methods EBRPACKAGESET_GET_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZEBR_DASHBOARD_DPC_EXT_1 IMPLEMENTATION.


  method EBRBUSET_GET_ENTITY.

  endmethod.


  METHOD EBRBUSET_GET_ENTITYSET.

*    SELECT DISTINCT bu_name INTO TABLE et_entityset FROM zebr_t_header. "#EC CI_NOWHERE
*    SORT et_entityset BY bu_name.
*    DELETE et_entityset WHERE  bu_name EQ ''.
  ENDMETHOD.


  METHOD EBRCOUNTSET_GET_ENTITY.

    SELECT  status,COUNT(*)  FROM zebr_t_header GROUP BY status INTO (@er_entity-status,@er_entity-count). "#EC CI_NOWHERE
    ENDSELECT.
  ENDMETHOD.


  METHOD EBRCOUNTSET_GET_ENTITYSET.
    SELECT  status, COUNT(*) AS count FROM zebr_t_header GROUP BY status
      INTO CORRESPONDING FIELDS OF TABLE @et_entityset.               "#EC CI_NOWHERE

  ENDMETHOD.


  METHOD EBRCREATEBYSET_GET_ENTITYSET.
    TYPES : BEGIN OF ty_emp,
              bname TYPE xubname,
              empid TYPE zempid,
            END OF ty_emp.
    TYPES: BEGIN OF ty_bname,
             bname TYPE xubname,
           END OF ty_bname.
    DATA: lt_account   TYPE STANDARD TABLE OF ty_emp,
          lt_user_temp TYPE STANDARD TABLE OF ty_bname.

    SELECT DISTINCT created_by INTO TABLE @DATA(lt_user) FROM zebr_t_header. "#EC CI_NOWHERE

    IF lt_user IS NOT INITIAL.
      lt_user_temp[] = lt_user[].
      SELECT bname, accnt INTO TABLE @DATA(lt_usr02) FROM usr02 FOR ALL ENTRIES IN
        @lt_user_temp WHERE bname EQ @lt_user_temp-bname.
      lt_account = lt_usr02.
      IF lt_account IS NOT INITIAL.
        SELECT employee_id,displayname INTO TABLE @DATA(lt_workday) FROM zworkday_data "#EC CI_NO_TRANSFORM
          FOR ALL ENTRIES IN @lt_account WHERE employee_id EQ @lt_account-empid.
      ENDIF.
*    ENDIF.

      LOOP AT lt_user INTO DATA(lw_user).
        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entity>).
        MOVE-CORRESPONDING lw_user TO <fs_entity>.
        READ TABLE lt_usr02 ASSIGNING FIELD-SYMBOL(<fs_usr02>) WITH KEY bname = lw_user-created_by.
        IF sy-subrc EQ 0.
          READ TABLE lt_workday ASSIGNING FIELD-SYMBOL(<fs_workday>) WITH KEY employee_id = <fs_usr02>-accnt.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING <fs_workday> TO <fs_entity>.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND INITIAL LINE TO et_entityset ASSIGNING <fs_entity>.
      SORT et_entityset BY  displayname.
    ENDIF.
    REFRESH: lt_account,lt_user_temp.
  ENDMETHOD.


  METHOD EBRDATASET_GET_ENTITY.
    DATA: ls_key_tab TYPE /iwbep/s_mgw_name_value_pair,
          lv_ebr     TYPE zebr_de_req_no.

* Get the key property values
    READ TABLE it_key_tab WITH KEY name = 'ReqNo' INTO ls_key_tab.
    IF sy-subrc = 0.
      lv_ebr = ls_key_tab-value.
    ENDIF.

    IF lv_ebr IS NOT INITIAL.
      SELECT SINGLE * INTO @DATA(ls_header) FROM zebr_t_header WHERE req_no EQ @lv_ebr.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING ls_header TO er_entity.
        er_entity-ebrsubmitteddate = ls_header-created_on.
        SELECT SINGLE bname,accnt INTO @DATA(lw_usr02) FROM usr02 WHERE bname EQ @ls_header-created_by.
        IF sy-subrc EQ 0.
          SELECT SINGLE displayname INTO @DATA(lv_name) FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
          IF sy-subrc EQ 0.
            er_entity-created_by_name = lv_name.
            er_entity-requestor_name = lv_name.
          ENDIF.
        ENDIF.

        SELECT SINGLE bname,accnt INTO @lw_usr02 FROM usr02 WHERE bname EQ @ls_header-npp_processor.
        IF sy-subrc EQ 0.
          SELECT SINGLE displayname INTO @DATA(lv_processor) FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
          IF sy-subrc EQ 0.
            er_entity-npp_processor_name = lv_processor.
          ENDIF.
        ENDIF.

        SELECT SINGLE * INTO @DATA(ls_lot) FROM zebr_t_lotinfo WHERE req_no EQ @lv_ebr.  "#EC CI_ALL_FIELDS_NEEDED
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING ls_lot TO er_entity.
          er_entity-mfrpn_item = ls_lot-mfrpn.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = ls_lot-asy_facility
            IMPORTING
              output = ls_lot-asy_facility.
          SELECT SINGLE name1 FROM lfa1 INTO  @DATA(lv_name2) WHERE lifnr EQ @ls_lot-asy_facility .
          IF sy-subrc EQ 0.
            er_entity-vendor = lv_name2.
          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR lv_ebr.
    ENDIF.


  ENDMETHOD.


  METHOD EBRDATASET_GET_ENTITYSET.
    TYPES:BEGIN OF ty_master.
        INCLUDE TYPE zebr_t_mast_data.
*    TYPES:mtart TYPE mtart.
    TYPES:END OF ty_master.

    TYPES: mtart_range           TYPE RANGE OF mara-mtart,
           mfrpn_range           TYPE RANGE OF zebr_t_lotinfo-mfrpn,
           lot_no_range          TYPE RANGE OF zebr_t_lotinfo-lot_no,
           output_die_type_range TYPE RANGE OF zebr_t_lotinfo-output_die_type,
           plan_no_range         TYPE RANGE OF zebr_t_lotinfo-planning_id.
    DATA:

      lt_part       TYPE TABLE OF          selopt,
      lt_ebr        TYPE TABLE OF          selopt,
      lt_status     TYPE TABLE OF          selopt,
      lt_lot        TYPE TABLE OF          selopt,
      lt_plan_id    TYPE TABLE OF           selopt,
      lt_die_type   TYPE TABLE OF           selopt,
      lt_created_by TYPE TABLE OF           selopt,
      lt_created_on TYPE TABLE OF           selopt,
      lt_planner    TYPE TABLE OF           selopt,
      lt_assembly   TYPE TABLE OF           selopt,
      lt_scc        TYPE TABLE OF           selopt,
      lt_max        TYPE TABLE OF           selopt,
      lt_ebrdata    TYPE zcl_zebr_dashboard_mpc=>tt_ebrdata,
      lt_npp_init   TYPE TABLE OF selopt,
      lt_bu         TYPE TABLE OF selopt,
      lt_req_type   TYPE TABLE OF selopt,
      lt_package    TYPE TABLE OF selopt,
      lt_rel        TYPE TABLE OF selopt,
      lt_pcn        TYPE TABLE OF selopt.

    DATA:
      ls_part       TYPE                   selopt,
      ls_ebr        TYPE                   selopt,
      ls_status     TYPE                   selopt,
      ls_lot        TYPE                   selopt,
      ls_plan_id    TYPE                   selopt,
      ls_die_type   TYPE                   selopt,
      ls_created_by TYPE                   selopt,
      ls_created_on TYPE                   selopt,
      ls_planner    TYPE                   selopt,
      ls_assembly   TYPE                   selopt,
      ls_scc        TYPE                   selopt,
      ls_max        TYPE                   selopt,
      ls_npp_init   TYPE               selopt,
      ls_bu         TYPE               selopt,
      ls_req_type   TYPE               selopt,
      ls_package    TYPE               selopt,
      ls_rel        TYPE  selopt,
      ls_pcn        TYPE  selopt.

    DATA: lt_master3 TYPE STANDARD TABLE OF ty_master,
          ls_master3 TYPE ty_master,
          lt_master4 TYPE STANDARD TABLE OF ty_master,
          ls_master4 TYPE ty_master,
          lt_master5 TYPE STANDARD TABLE OF ty_master,
          ls_master5 TYPE ty_master,
          lt_mat     TYPE STANDARD TABLE OF ty_master.
    TYPES:BEGIN OF ty_usr02,
            bname TYPE xubname,
            accnt TYPE zempid,
          END OF ty_usr02.

    DATA:lt_user02 TYPE STANDARD TABLE OF ty_usr02,
         ls_user02 TYPE ty_usr02.

    DATA : lrt_reqno             TYPE RANGE OF zebr_de_req_no,
           lv_ov_gts_clfs        TYPE char01,
           lv_ov_material_status TYPE char01,
           lv_ov_pv_status       TYPE char01,
           lv_ov_route_status    TYPE char01,
           lv_ov_bom_status      TYPE char01,
           lv_lot                TYPE string.

    CONSTANTS:lc_00 TYPE char2 VALUE '00',
              lc_01 TYPE char2 VALUE '01',
              lc_99 TYPE char2 VALUE '99',
              lc_02 TYPE char2 VALUE '02'.
    REFRESH  lt_user02 .

    " Get Filter values

    IF it_filter_select_options IS NOT INITIAL.
      LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options).
        CASE ls_filter_select_options-property .
          WHEN 'PartNumber'.
            LOOP AT ls_filter_select_options-select_options INTO DATA(ls_so) WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_part.

              INSERT ls_part INTO TABLE lt_part.
              CLEAR ls_part.
            ENDLOOP.
          WHEN 'ReqNo'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_ebr.
              INSERT ls_ebr INTO TABLE lt_ebr.
              CLEAR ls_ebr.
            ENDLOOP.
          WHEN 'Status'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_status.
              INSERT ls_status INTO TABLE lt_status.
              CLEAR ls_status.
            ENDLOOP.
          WHEN 'AssemblyPending'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_assembly.
              INSERT ls_assembly INTO TABLE lt_assembly.
              CLEAR ls_assembly.
            ENDLOOP.
          WHEN 'SccPending'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_scc.
              INSERT ls_scc INTO TABLE lt_scc.
              CLEAR ls_scc.
            ENDLOOP.
          WHEN 'LotNo'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_lot.
              INSERT ls_lot INTO TABLE  lt_lot.
              CLEAR ls_lot.
            ENDLOOP.
          WHEN 'LotDieType'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_die_type.
              INSERT ls_die_type INTO TABLE lt_die_type.
              CLEAR ls_die_type.
            ENDLOOP.
          WHEN 'PlanningId'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_plan_id.
              INSERT ls_plan_id INTO TABLE lt_plan_id.
              CLEAR ls_plan_id.
            ENDLOOP.
          WHEN 'CreatedBy'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_created_by.
              INSERT ls_created_by INTO TABLE lt_created_by.
              CLEAR ls_created_by.
            ENDLOOP.
          WHEN 'CreatedOn'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_created_on.
              INSERT ls_created_on INTO TABLE lt_created_on.
              CLEAR ls_created_on.
            ENDLOOP.
          WHEN 'NppPlanner'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_planner.

              INSERT ls_planner INTO TABLE lt_planner.

              CLEAR ls_planner.
            ENDLOOP.
          WHEN 'MaxRecords'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_max.
              INSERT ls_max INTO TABLE lt_max.
*              CLEAR ls_max.
            ENDLOOP.
          WHEN 'NppInitial'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_npp_init.
              INSERT ls_npp_init INTO TABLE lt_npp_init.
              CLEAR ls_npp_init.
            ENDLOOP.
          WHEN 'BuName'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_bu.
              INSERT ls_bu INTO TABLE lt_bu.
              CLEAR ls_bu.
            ENDLOOP.
          WHEN 'ReqType'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_req_type.
              INSERT ls_req_type INTO TABLE lt_req_type.
              CLEAR ls_req_type.
            ENDLOOP.
          WHEN 'PackageType'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_package.
              INSERT ls_package INTO TABLE lt_package.
              CLEAR ls_package.
            ENDLOOP.
          WHEN 'PcnNo'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_pcn.
              INSERT ls_pcn INTO TABLE lt_pcn.
              CLEAR ls_pcn.
            ENDLOOP.
          WHEN 'Relid'.
            LOOP AT ls_filter_select_options-select_options INTO ls_so WHERE low IS NOT INITIAL.
              MOVE-CORRESPONDING ls_so TO ls_rel.
              INSERT ls_rel INTO TABLE lt_rel.
              CLEAR ls_rel.
            ENDLOOP.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    " Validate Authority Check

    DATA(lv_user) = sy-uname.

    " NPP User
    CALL FUNCTION 'AUTHORITY_CHECK'
      EXPORTING
*       new_buffering       = 3
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
    IF sy-subrc NE 2.
      DATA(npp_flag) = ''.
    ELSE.
      npp_flag = 'X'.
      SELECT SINGLE bname,accnt INTO @DATA(lw_usr02) FROM usr02 WHERE bname EQ @lv_user.
      IF sy-subrc EQ 0.
        SELECT SINGLE displayname INTO @DATA(lv_name) FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
        IF sy-subrc EQ 0.
          DATA(npp_name) = lv_name.
          DATA(npp_user) = lv_user.
        ENDIF.
      ENDIF.
    ENDIF.

    " ASY user
    CALL FUNCTION 'AUTHORITY_CHECK'
      EXPORTING
*       new_buffering       = 3
        user                = sy-uname
        object              = 'ZEBR_OBJ'
        field1              = 'ZEBR_ROLE'
        value1              = 'ASY'
      EXCEPTIONS
        user_dont_exist     = 1
        user_is_authorized  = 2
        user_not_authorized = 3
        user_is_locked      = 4
        OTHERS              = 5.
    IF sy-subrc NE 2.
      DATA(asy_flag) = ''.
    ELSE.
      asy_flag = 'X'.
      SELECT SINGLE bname,accnt INTO @lw_usr02 FROM usr02 WHERE bname EQ @lv_user.
      IF sy-subrc EQ 0.
        SELECT SINGLE displayname INTO @lv_name FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
        IF sy-subrc EQ 0.
          DATA(asy_name) = lv_name.
          DATA(asy_user) = lv_user.
        ENDIF.
      ENDIF.
    ENDIF.

    " Requestor

    CALL FUNCTION 'AUTHORITY_CHECK'
      EXPORTING
*       new_buffering       = 3
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
    IF sy-subrc NE 2.
      DATA(req_flag) = ''.
    ELSE.
      req_flag = 'X'.
      SELECT SINGLE bname,accnt INTO @lw_usr02 FROM usr02 WHERE bname EQ @lv_user.
      IF sy-subrc EQ 0.
        SELECT SINGLE displayname INTO @lv_name FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
        IF sy-subrc EQ 0.
          DATA(req_name) = lv_name.
          DATA(req_user) = lv_user.
        ENDIF.
      ENDIF.
    ENDIF.

    " SCC User
    CALL FUNCTION 'AUTHORITY_CHECK'
      EXPORTING
*       new_buffering       = 3
        user                = sy-uname
        object              = 'ZEBR_OBJ'
        field1              = 'ZEBR_ROLE'
        value1              = 'SCC'
      EXCEPTIONS
        user_dont_exist     = 1
        user_is_authorized  = 2
        user_not_authorized = 3
        user_is_locked      = 4
        OTHERS              = 5.
    IF sy-subrc NE 2.
      DATA(scc_flag) = ''.
    ELSE.
      scc_flag = 'X'.
      SELECT SINGLE bname,accnt INTO @lw_usr02 FROM usr02 WHERE bname EQ @lv_user.
      IF sy-subrc EQ 0.
        SELECT SINGLE displayname INTO @lv_name FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
        IF sy-subrc EQ 0.
          DATA(scc_name) = lv_name.
          DATA(scc_user) = lv_user.
        ENDIF.
      ENDIF.
    ENDIF.

    " SCM User

    CALL FUNCTION 'AUTHORITY_CHECK'
      EXPORTING
*       new_buffering       = 3
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
    IF sy-subrc NE 2.
      DATA(scm_flag) = ''.
    ELSE.
      scm_flag = 'X'.
      SELECT SINGLE bname,accnt INTO @lw_usr02 FROM usr02 WHERE bname EQ @lv_user.
      IF sy-subrc EQ 0.
        SELECT SINGLE displayname INTO @lv_name FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
        IF sy-subrc EQ 0.
          DATA(scm_name) = lv_name.
          DATA(scm_user) = lv_user.
        ENDIF.
      ENDIF.
    ENDIF.

    IF asy_flag = 'X'.
      DATA(flag) = 'A'.
    ENDIF.
    IF scc_flag = 'X'.
      flag = 'S'.
    ENDIF.
    IF asy_flag = 'X' AND scc_flag = 'X'.
      flag = 'AS'.
    ENDIF.

    DATA(mtart_range) = VALUE mtart_range(
        ( sign = 'I' option = 'EQ' low = 'ZASY' high = '') ( sign = 'I' option = 'EQ' low = 'ZBMP' high = '') ).

    IF  is_paging-top > 0 .
      DATA(lv_rowcount)  =  is_paging-top  + is_paging-skip.
    ELSE.
      lv_rowcount = '10'.
    ENDIF.

    " Get User data from ZWORKDAY_DATA table

    SELECT bname,accnt INTO TABLE @DATA(lt_user) FROM usr02. "#EC CI_GENBUFF
    IF sy-subrc EQ 0.
      lt_user02 = lt_user.
      IF lt_user02 IS NOT INITIAL.
        SELECT employee_id,sapusername,displayname,primary_email INTO TABLE @DATA(lt_work) FROM zworkday_data "#EC CI_NO_TRANSFORM
          FOR ALL ENTRIES IN @lt_user02 WHERE employee_id EQ  @lt_user02-accnt.
      ENDIF.
    ENDIF.

    " Reuest Type Description

    SELECT DISTINCT field1,
                    field2,
                    field3 INTO TABLE @DATA(lt_req_type_desc)
                    FROM zebr_config
                    WHERE name EQ 'REQ_TYPE' AND type EQ 'DROP_DOWN'.

    " request status
    SELECT  name , type , field1 ,field2  FROM zebr_config
       INTO  TABLE  @DATA(lt_req_status) WHERE name    = 'REQ_STATUS'
                                            AND type    = 'DROP_DOWN'.

*    SELECT * INTO TABLE @DATA(lt_request) FROM zebr_config
*                    WHERE name EQ 'REQ_TYPE' AND type EQ 'DROP_DOWN'.


    " Fasttrack Description
    SELECT DISTINCT field1,
                    field2
                    INTO TABLE @DATA(lt_fasttrack)
      FROM zebr_config WHERE name EQ 'FASTTRACK' AND   type EQ 'DROP_DOWN'.


    IF lt_lot IS NOT INITIAL OR lt_die_type IS NOT INITIAL OR lt_part IS NOT INITIAL OR lt_plan_id IS NOT INITIAL.

      READ TABLE lt_part INTO DATA(ls_partpat) INDEX 1.
      IF ls_partpat-low CS '*'.
        DATA(part_flag) = 'X'.
        DATA(mfrpn_range) = VALUE mfrpn_range( ( sign = 'I' option = 'CP' low = ls_partpat-low high = '') ).
      ENDIF.

      READ TABLE lt_lot INTO DATA(ls_lotpat) INDEX 1.

      IF ls_lotpat-low CS '*'.
        DATA(lot_flag) = 'X'.
        DATA(lot_no_range) = VALUE lot_no_range( ( sign = 'I' option = 'CP' low = ls_lotpat-low high = '') ).
      ENDIF.

      READ TABLE lt_die_type INTO DATA(ls_diepat) INDEX 1.

      IF ls_diepat-low CS '*'.
        DATA(die_flag) = 'X'.
        DATA(output_die_type_range) = VALUE output_die_type_range( ( sign = 'I' option = 'CP' low = ls_diepat-low high = '') ).
      ENDIF.

      READ TABLE lt_plan_id INTO DATA(ls_planpat) INDEX 1.
      IF ls_planpat-low CS '*'.
        DATA(plan_flag) = 'X'.
        DATA(plan_no_range) = VALUE plan_no_range( ( sign = 'I' option = 'CP' low = ls_planpat-low high = '') ).
      ENDIF.


      " If filter search criteria either part number or Lot Number or Die Type

      IF part_flag IS NOT INITIAL OR lot_flag IS NOT INITIAL OR die_flag IS NOT INITIAL OR plan_flag IS NOT INITIAL.
        SELECT * INTO TABLE @DATA(lt_lot3) UP TO @ls_max-low ROWS FROM zebr_t_lotinfo "#EC CI_NOFIRST
         WHERE  lot_no IN @lot_no_range AND mfrpn  IN @mfrpn_range AND output_die_type IN @output_die_type_range
         AND planning_id IN @plan_no_range
         ORDER BY  created_on DESCENDING.               "#EC CI_NOFIELD
      ELSE.
        SELECT * INTO TABLE @lt_lot3 UP TO @ls_max-low ROWS FROM zebr_t_lotinfo "#EC CI_NOFIRST
          WHERE  lot_no IN @lt_lot AND mfrpn  IN @lt_part AND output_die_type IN @lt_die_type
          AND planning_id IN @lt_plan_id
          ORDER BY  created_on DESCENDING.              "#EC CI_NOFIELD
      ENDIF.
      SORT lt_lot3 BY req_no.
      DATA(lt_lot5) = lt_lot3.
      DELETE ADJACENT DUPLICATES FROM lt_lot3 COMPARING req_no.
      IF lt_lot3 IS NOT INITIAL.
        " Get Header Data
        SELECT * INTO TABLE @DATA(lt_header3) FROM zebr_t_header FOR ALL ENTRIES IN
          @lt_lot3 WHERE req_no EQ @lt_lot3-req_no AND req_type IN @lt_req_type AND
          req_status_no IN @lt_status AND
          bu_name IN @lt_bu AND package_type IN @lt_package AND
            npp_processor IN @lt_planner AND
          issue_assy_team IN @lt_assembly AND issue_pdm_team IN @lt_scc
          AND pcn_no IN @lt_pcn  AND relid IN @lt_rel
      AND created_by IN @lt_created_by AND created_on IN @lt_created_on
       .
        IF lt_header3 IS NOT INITIAL.
          SELECT * INTO TABLE @DATA(lt_mast3) FROM zebr_t_mast_data FOR ALL ENTRIES IN "#EC CI_NO_TRANSFORM
          @lt_header3 WHERE req_no EQ @lt_header3-req_no .
          IF lt_mast3 IS NOT INITIAL.
            DATA(lt_mast3_temp) = lt_mast3.
            DELETE ADJACENT DUPLICATES FROM lt_mast3_temp COMPARING  matnr. "#EC CI_SORTED
            SELECT matnr, mtart, matkl INTO TABLE @DATA(lt_mara) FROM mara "#EC CI_NO_TRANSFORM
              FOR ALL ENTRIES IN @lt_mast3_temp  WHERE matnr EQ @lt_mast3_temp-matnr.
          ENDIF.
        ENDIF.

        LOOP AT lt_mast3 INTO DATA(ls_mast3).
          MOVE-CORRESPONDING ls_mast3 TO ls_master3.
          IF ls_master3-mtart IS INITIAL.
            READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = ls_mast3-matnr.
            ls_master3-mtart = ls_mara-mtart.
          ENDIF.
          APPEND ls_master3 TO lt_master3.
          CLEAR:ls_master3.

        ENDLOOP.
        SORT lt_lot3 BY req_no.
        SORT lt_lot5 BY req_no.
        SORT lt_header3 BY req_no.
        SORT lt_master3 BY req_no.

        LOOP AT lt_lot3 INTO DATA(ls_lot3).
          READ TABLE lt_lot5 INTO DATA(ls_lotadd2) WITH  KEY req_no = ls_lot3-req_no.
          IF sy-subrc EQ 0.
            DATA(v_index2) = sy-tabix.
          ENDIF.
          LOOP AT lt_lot5 INTO ls_lotadd2 FROM v_index2.
            IF ls_lot3-req_no <> ls_lotadd2-req_no.
              EXIT.
            ENDIF.
            CONCATENATE  ls_lotadd2-lot_no lv_lot INTO lv_lot SEPARATED BY ','.
            CLEAR ls_lotadd2.
          ENDLOOP.
          CLEAR v_index2.
          READ TABLE lt_header3 INTO DATA(ls_header3) WITH KEY req_no = ls_lot3-req_no BINARY SEARCH.
          IF sy-subrc EQ 0.


            APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entity>).
            MOVE-CORRESPONDING ls_header3 TO <fs_entity>.
            <fs_entity>-ebrsubmitteddate = ls_header3-created_on.
            " Claim button
            IF ( scm_flag EQ 'X' OR npp_flag EQ 'X' ) AND
      ls_header3-req_status_no EQ lc_01 AND ls_header3-npp_processor IS INITIAL.
              <fs_entity>-cenabled = abap_true.
              <fs_entity>-claimtxt = 'Claim'.
            ELSEIF ( scm_flag EQ 'X' OR npp_flag EQ 'X' ) AND
      ls_header3-req_status_no >= lc_01  AND ls_header3-npp_processor IS NOT INITIAL..
              <fs_entity>-cenabled = abap_false.
              <fs_entity>-claimtxt = 'Claimed'.
            ELSE.
              <fs_entity>-cenabled = abap_false.
              <fs_entity>-claimtxt = 'Claim' .             "#EC NO_TEXT
            ENDIF.
            READ TABLE lt_req_type_desc INTO DATA(ls_req_type_desc) WITH KEY
            field1 = ls_header3-req_type.
            IF sy-subrc EQ 0.
              <fs_entity>-req_type_desc = ls_req_type_desc-field2.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input  = ls_req_type_desc-field3
                IMPORTING
                  output = ls_req_type_desc-field3.

              READ TABLE lt_fasttrack INTO DATA(ls_fasttrack) WITH KEY
              field2 = ls_req_type_desc-field3.
              IF sy-subrc EQ 0.
                <fs_entity>-support_level = ls_fasttrack-field1.
              ENDIF.
            ENDIF.
            " request  no description
            READ TABLE  lt_req_status INTO DATA(ls_req_status)  WITH KEY  name    = 'REQ_STATUS'
                                                                                type    = 'DROP_DOWN'
                                                                                field1  =    ls_header3-req_status_no .
            IF sy-subrc EQ 0.
              <fs_entity>-req_status   =  ls_req_status-field2 .
              CLEAR ls_req_status .
            ENDIF .


            READ TABLE lt_user02 INTO ls_user02 WITH KEY bname = ls_header3-created_by.
            IF sy-subrc EQ 0.
              READ TABLE lt_work INTO DATA(ls_work) WITH KEY employee_id = ls_user02-accnt.
              IF sy-subrc EQ 0.
                <fs_entity>-created_by_name = ls_work-displayname.
                <fs_entity>-requestor_name = ls_work-displayname.
              ENDIF.
              CLEAR:ls_user02,ls_work.
            ENDIF.

            IF ls_header3-npp_processor IS NOT INITIAL.
              READ TABLE lt_user02 INTO ls_user02 WITH KEY bname = ls_header3-npp_processor.
              IF sy-subrc EQ 0.
                READ TABLE lt_work INTO ls_work WITH KEY employee_id = ls_user02-accnt.
                IF sy-subrc EQ 0.
                  <fs_entity>-npp_processor_name = ls_work-displayname.
                ENDIF.
              ENDIF.
              CLEAR:ls_user02,ls_work.
            ENDIF.
            IF npp_flag = 'X'.
              <fs_entity>-npp_flag = 'X'.
              <fs_entity>-npp_name = npp_name.

            ELSE.
              <fs_entity>-npp_flag = ''.
            ENDIF.
            IF req_flag IS NOT INITIAL.
              <fs_entity>-req_name = req_name.
              <fs_entity>-req_flag = 'X'.
            ELSE.
              <fs_entity>-req_flag = ''.
            ENDIF.
            IF asy_flag IS NOT INITIAL.
              <fs_entity>-asy_name = asy_name.
              <fs_entity>-asy_flag = 'X'.
            ELSE.
              <fs_entity>-asy_flag = ''.
            ENDIF.
            IF scc_flag IS NOT INITIAL.
              <fs_entity>-scc_name = scc_name.
              <fs_entity>-scc_flag = 'X'.
            ELSE.
              <fs_entity>-scc_flag = ''.
            ENDIF.
            IF scm_flag IS NOT INITIAL.
              <fs_entity>-scm_flag = 'X'.
              <fs_entity>-scm_name = scm_name.

            ELSE.
              <fs_entity>-scm_flag = ''.
            ENDIF.


*            <fs_entity>-requestor_name = sy-uname.
*            MOVE-CORRESPONDING ls_lot3 TO <fs_entity>.
            IF  ls_lot3  IS NOT INITIAL.
              <fs_entity>-mfrpn_item = ls_lot3-mfrpn.
              SHIFT lv_lot RIGHT DELETING TRAILING ','.
              <fs_entity>-lot_con = lv_lot.
              CONCATENATE ls_lot3-output_die_type '-' ls_lot3-output_die_rev INTO  DATA(lv_output_die).
              SHIFT lv_output_die RIGHT DELETING TRAILING '-'.
              <fs_entity>-output_die = lv_output_die.
*            <fs_entity>-lot_con = ls_lot3-lot_no.
              IF ls_lot3-asy_facility IS NOT INITIAL.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = ls_lot3-asy_facility
                  IMPORTING
                    output = ls_lot3-asy_facility.

                SELECT SINGLE name1 FROM lfa1 INTO @DATA(lv_name1) WHERE lifnr EQ @ls_lot3-asy_facility .
                IF sy-subrc EQ 0.
                  <fs_entity>-vendor = lv_name1.
                ENDIF.
              ELSE.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = ls_lot3-bump_facility
                  IMPORTING
                    output = ls_lot3-bump_facility.

                SELECT SINGLE name1 FROM lfa1 INTO @lv_name1 WHERE lifnr EQ @ls_lot3-bump_facility .
                IF sy-subrc EQ 0.
                  <fs_entity>-vendor = lv_name1.
                ENDIF.
              ENDIF.
            ENDIF.
*            LOOP AT lt_master3 INTO ls_master3 WHERE req_no EQ ls_header3-req_no.

            CASE flag.
              WHEN 'A'.
                DELETE lt_master3 WHERE NOT mtart IN mtart_range.
              WHEN 'S'.
                DELETE lt_master3 WHERE  mtart IN mtart_range.
              WHEN 'AS'.

            ENDCASE.
            " Get Material Status, BOM Status and Route Status

            IF asy_flag = 'X' OR scc_flag = 'X'.
              LOOP AT  lt_master3 INTO ls_master3  WHERE  material_status =   'C' OR
*                                                                   material_status =   'W1' OR
                                                                   material_status =   'W3' OR
                                                                   material_status =   'W4' OR
                                                                   material_status =   'X0'  .
                lv_ov_material_status  =  'R'   .
                IF lv_ov_material_status  IS NOT INITIAL .
                  EXIT .
                ENDIF.
              ENDLOOP.
              IF lv_ov_material_status IS INITIAL .
                LOOP AT  lt_master3  INTO ls_master3  WHERE  material_status =   'P' OR
                                                           material_status =   'Q' OR
                                                           material_status =   'W2'  .
*                  lv_ov_material_status  =  'R'   .
                  lv_ov_material_status  =  'G'   .
                  IF lv_ov_material_status  IS NOT INITIAL .
                    EXIT .
                  ENDIF.
                ENDLOOP.
                IF lv_ov_material_status IS INITIAL .
                  LOOP AT  lt_master3  INTO ls_master3  WHERE  material_status =   'R1' OR
                                                             material_status =   'R2' OR
                                                              material_status =   'R' OR " JGAND37207
                                                             material_status =   'W1'. " JGAND37207
                    lv_ov_material_status  =  'G'   .
                    IF lv_ov_material_status  IS NOT INITIAL .
                      EXIT .
                    ENDIF.
                  ENDLOOP.
                ENDIF.
              ENDIF.

              " bom status
              LOOP AT  lt_master3  INTO ls_master3  WHERE  bom_status =   '01' OR
                                                               bom_status =   '04'   .
                lv_ov_bom_status  =  'R'   .
                IF lv_ov_bom_status  IS NOT INITIAL .
                  EXIT .
                ENDIF.
              ENDLOOP.
              IF lv_ov_bom_status IS INITIAL .
                READ TABLE lt_master3   INTO ls_master3  WITH KEY  bom_status = '02'  .
                IF sy-subrc EQ 0 .
*                  lv_ov_bom_status  =  'R'  .
                  lv_ov_bom_status  =  'G'  .
                ELSE.
                  READ TABLE lt_master3   INTO ls_master3  WITH KEY  bom_status = '03'  .
                  IF sy-subrc EQ 0 .
                    lv_ov_bom_status  =  'G'  .
                  ENDIF.
                ENDIF.
              ENDIF.
              " Route status
              LOOP AT  lt_master3  INTO ls_master3  WHERE  route_status =   '01' OR route_status =   '1'  OR
                                                           route_status =   '05' OR  route_status =   '5'  OR
                                                           route_status =   '03'  OR route_status =   '3'   .
                lv_ov_route_status  =  'R'   .
                IF lv_ov_route_status  IS NOT INITIAL .
                  EXIT .
                ENDIF.
              ENDLOOP.
              IF lv_ov_route_status IS INITIAL .
                READ TABLE lt_master3   INTO ls_master3  WITH KEY  route_status = '04'  .
                IF sy-subrc EQ 0 .
                  lv_ov_route_status  =  'G'  .
                ENDIF.
              ENDIF.


              <fs_entity>-material_status = lv_ov_material_status.
              <fs_entity>-route_status = lv_ov_route_status.
              <fs_entity>-bom_status = lv_ov_bom_status.
            ENDIF.
          ENDIF.
          CLEAR: lv_ov_material_status,
                 lv_ov_route_status,
                 lv_ov_bom_status,
                 lv_lot.
        ENDLOOP.

      ENDIF.

*      IF npp_flag = 'X'.
*        SORT et_entityset BY npp_processor_name req_no.
*      ELSE.
      SORT et_entityset BY req_no DESCENDING.
*      ENDIF.

      " If search criteria Other than Part number, Lot number and Die Type
    ELSE.

      " Initial load for SCM user
      IF lt_npp_init IS NOT INITIAL AND  scm_flag = 'X'.

        SELECT * INTO TABLE @DATA(lt_header4) UP TO @ls_max-low ROWS FROM zebr_t_header "#EC CI_NOFIELD
            WHERE   req_status_no NE @lc_99 AND
         ( npp_processor EQ @sy-uname OR created_by EQ @sy-uname ) AND
          created_on IN @lt_created_on ORDER BY  created_on DESCENDING.
*        DELETE lt_header4 WHERE req_status_no EQ lc_00.

        " Initial load for NPP user
      ELSEIF lt_npp_init IS NOT INITIAL AND  npp_flag = 'X'.

        SELECT * INTO TABLE @lt_header4 UP TO @ls_max-low ROWS FROM zebr_t_header "#EC CI_NOFIELD
            WHERE    ( req_status_no EQ @lc_01 OR req_status_no EQ @lc_02 ) AND
          ( npp_processor EQ @sy-uname OR npp_processor EQ '' )  AND

          created_on IN @lt_created_on ORDER BY  created_on DESCENDING.

        DELETE lt_header4 WHERE req_status_no EQ lc_02 AND npp_processor NE sy-uname.

*        DELETE lt_header4 WHERE req_status_no EQ lc_00 OR req_status_no EQ ''.

        " Initial load for Asy user
      ELSEIF lt_npp_init IS NOT INITIAL AND  asy_flag = 'X'.

        SELECT * INTO TABLE @lt_header4 UP TO @ls_max-low ROWS FROM zebr_t_header "#EC CI_NOFIELD
      WHERE issue_assy_team EQ 'X' AND created_on IN @lt_created_on ORDER BY  created_on DESCENDING.


        " Initial load for Requestor user
      ELSEIF lt_npp_init IS NOT INITIAL AND  req_flag = 'X'.

        SELECT * INTO TABLE @lt_header4 UP TO @ls_max-low ROWS FROM zebr_t_header "#EC CI_NOFIELD
      WHERE created_by EQ @sy-uname AND created_on IN @lt_created_on ORDER BY  created_on DESCENDING.

        " Initial load for SCC user
      ELSEIF lt_npp_init IS NOT INITIAL AND  scc_flag = 'X'.

        SELECT * INTO TABLE @lt_header4 UP TO @ls_max-low ROWS FROM zebr_t_header "#EC CI_NOFIELD
   WHERE issue_pdm_team EQ 'X' AND created_on IN @lt_created_on ORDER BY  created_on DESCENDING.
*        ELSE.
*          SELECT * INTO TABLE @lt_header4 UP TO @lv_rowcount ROWS FROM zebr_t_header
*WHERE issue_pdm_team EQ 'X'.
*        ENDIF.
*      ELSEIF lt_npp_init IS INITIAL AND  npp_flag = 'X'..
*        SELECT * INTO TABLE @lt_header4 UP TO @ls_max-low ROWS FROM zebr_t_header
*        WHERE req_no IN @lt_ebr
*        AND req_status_no NE @lc_00
*        AND issue_assy_team IN @lt_assembly AND issue_pdm_team IN @lt_scc
*        AND created_by IN @lt_created_by AND created_on IN @lt_created_on
*          AND npp_processor IN @lt_planner.
      ELSE.

        SELECT * INTO TABLE @lt_header4 UP TO @ls_max-low ROWS FROM zebr_t_header
        WHERE req_no IN @lt_ebr  AND req_type IN @lt_req_type
            AND req_status_no IN @lt_status
          AND bu_name IN @lt_bu AND package_type IN @lt_package
            AND npp_processor IN @lt_planner
        AND issue_assy_team IN @lt_assembly AND issue_pdm_team IN @lt_scc
          AND pcn_no IN @lt_pcn  AND relid IN @lt_rel
        AND created_by IN @lt_created_by AND created_on IN @lt_created_on
           ORDER BY  created_on DESCENDING.

      ENDIF.
      IF lt_header4 IS NOT INITIAL.

        " Get Lot information
        SELECT * INTO TABLE @DATA(lt_lot4) FROM zebr_t_lotinfo "#EC CI_NO_TRANSFORM
          FOR ALL ENTRIES IN @lt_header4 WHERE req_no = @lt_header4-req_no.

        SELECT * INTO TABLE @DATA(lt_mast4) FROM zebr_t_mast_data FOR ALL ENTRIES IN "#EC CI_NO_TRANSFORM
        @lt_header4 WHERE req_no EQ @lt_header4-req_no .
        IF lt_mast4 IS NOT INITIAL.
          DATA(lt_mast4_temp) = lt_mast4.
          DELETE ADJACENT DUPLICATES FROM lt_mast4_temp COMPARING  matnr. "#EC CI_SORTED
          SELECT matnr, mtart, matkl INTO TABLE @lt_mara FROM mara "#EC CI_NO_TRANSFORM
            FOR ALL ENTRIES IN @lt_mast4_temp  WHERE matnr EQ @lt_mast4_temp-matnr.
        ENDIF.
      ENDIF.

      LOOP AT lt_mast4 INTO DATA(ls_mast4).
        MOVE-CORRESPONDING ls_mast4 TO ls_master4.
        IF ls_master4-mtart IS INITIAL.
          READ TABLE lt_mara INTO ls_mara WITH KEY matnr = ls_mast4-matnr.
          ls_master4-mtart = ls_mara-mtart.
        ENDIF.
        APPEND ls_master4 TO lt_master4.
        CLEAR:ls_master4.
      ENDLOOP.

      SORT lt_lot4 BY req_no.
      SORT lt_header4 BY req_no.
      SORT lt_master4 BY req_no.


      LOOP AT lt_header4 INTO DATA(ls_header4).

        READ TABLE lt_lot4 INTO DATA(ls_lotadd) WITH  KEY req_no = ls_header4-req_no.
        IF sy-subrc EQ 0.
          DATA(v_index) = sy-tabix.
        ENDIF.
        LOOP AT lt_lot4 INTO ls_lotadd FROM v_index.
          IF ls_header4-req_no <> ls_lotadd-req_no.
            EXIT.
          ENDIF.
          CONCATENATE  ls_lotadd-lot_no lv_lot INTO lv_lot SEPARATED BY ','.
          CLEAR ls_lotadd.
        ENDLOOP.
        CLEAR v_index.
        READ TABLE lt_master4 INTO DATA(ls_mat) WITH  KEY req_no = ls_header4-req_no.
        IF sy-subrc EQ 0.
          DATA(v_index3) = sy-tabix.
        ENDIF.
        LOOP AT lt_master4 INTO ls_mat FROM v_index3.
          IF ls_header4-req_no <> ls_mat-req_no.
            EXIT.
          ENDIF.
          APPEND ls_mat TO lt_mat.
          CLEAR ls_mat.
        ENDLOOP.
        CLEAR v_index3.
        READ TABLE lt_lot4 INTO DATA(ls_lot4) WITH KEY req_no = ls_header4-req_no.
*        IF sy-subrc EQ 0.

        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entity2>).
        MOVE-CORRESPONDING ls_header4 TO <fs_entity2>.
        <fs_entity2>-ebrsubmitteddate = ls_header4-created_on.
        " Claim button
        IF ( scm_flag EQ 'X' OR npp_flag EQ 'X' ) AND
          ls_header4-req_status_no EQ lc_01 AND ls_header4-npp_processor IS INITIAL.
          <fs_entity2>-cenabled = abap_true.
          <fs_entity2>-claimtxt = 'Claim'.
        ELSEIF ( scm_flag EQ 'X' OR npp_flag EQ 'X' ) AND
           ls_header4-req_status_no >= lc_01
           AND ls_header4-npp_processor IS NOT INITIAL..
          <fs_entity2>-cenabled = abap_false.
          <fs_entity2>-claimtxt = 'Claimed'.
        ELSE.
          <fs_entity2>-cenabled = abap_false.
          <fs_entity2>-claimtxt = 'Claim'.
        ENDIF.

        READ TABLE lt_req_type_desc INTO ls_req_type_desc WITH KEY
          field1 = ls_header4-req_type.
        IF sy-subrc EQ 0.
          <fs_entity2>-req_type_desc = ls_req_type_desc-field2.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = ls_req_type_desc-field3
            IMPORTING
              output = ls_req_type_desc-field3.
          READ TABLE lt_fasttrack INTO ls_fasttrack WITH KEY
           field2 = ls_req_type_desc-field3.
          IF sy-subrc EQ 0.
            <fs_entity2>-support_level = ls_fasttrack-field1.
          ENDIF.

        ENDIF.
        " request  no description
        READ TABLE  lt_req_status INTO ls_req_status  WITH KEY  name    = 'REQ_STATUS'
                                                                            type    = 'DROP_DOWN'
                                                                            field1  =    ls_header4-req_status_no .
        IF sy-subrc EQ 0.
          <fs_entity2>-req_status   =  ls_req_status-field2 .
          CLEAR ls_req_status .
        ENDIF .
        READ TABLE lt_user02 INTO ls_user02 WITH KEY bname = ls_header4-created_by.
        IF sy-subrc EQ 0.
          READ TABLE lt_work INTO ls_work WITH KEY employee_id = ls_user02-accnt.
          IF sy-subrc EQ 0.
            <fs_entity2>-created_by_name = ls_work-displayname.
            <fs_entity2>-requestor_name = ls_work-displayname.
          ENDIF.
          CLEAR:ls_user02,ls_work.
        ENDIF.

        IF ls_header4-npp_processor IS NOT INITIAL.
          READ TABLE lt_user02 INTO ls_user02 WITH KEY bname = ls_header4-npp_processor.
          IF sy-subrc EQ 0.
            READ TABLE lt_work INTO ls_work WITH KEY employee_id = ls_user02-accnt.
            IF sy-subrc EQ 0.
              <fs_entity2>-npp_processor_name = ls_work-displayname.

            ENDIF.
            CLEAR:ls_user02,ls_work.
          ENDIF.
        ENDIF.
        IF npp_flag IS NOT INITIAL.
          <fs_entity2>-npp_flag = 'X'.
          <fs_entity2>-npp_name = npp_name.

        ELSE.
          <fs_entity2>-npp_flag = ''.
        ENDIF.
        IF req_flag IS NOT INITIAL.
          <fs_entity2>-req_name = req_name.
          <fs_entity2>-req_flag = 'X'.
        ELSE.
          <fs_entity2>-req_flag = ''.
        ENDIF.
        IF asy_flag IS NOT INITIAL.
          <fs_entity2>-asy_name = asy_name.
          <fs_entity2>-asy_flag = 'X'.
        ELSE.
          <fs_entity2>-asy_flag = ''.
        ENDIF.
        IF scc_flag IS NOT INITIAL.
          <fs_entity2>-scc_name = scc_name.
          <fs_entity2>-scc_flag = 'X'.
        ELSE.
          <fs_entity2>-scc_flag = ''.
        ENDIF.

        IF scm_flag IS NOT INITIAL.
          <fs_entity2>-scm_flag = 'X'.
          <fs_entity2>-scm_name = scm_name.

        ELSE.
          <fs_entity2>-scm_flag = ''.
        ENDIF.

*            <fs_entity2>-requestor_name = sy-uname.
        IF  ls_lot4 IS NOT INITIAL.
*          MOVE-CORRESPONDING ls_lot4 TO <fs_entity2>.
          <fs_entity2>-mfrpn_item = ls_lot4-mfrpn.
          SHIFT lv_lot RIGHT DELETING TRAILING ','.
          <fs_entity2>-lot_con = lv_lot.

          CONCATENATE ls_lot4-output_die_type '-' ls_lot4-output_die_rev INTO lv_output_die.
          SHIFT lv_output_die RIGHT DELETING TRAILING '-'.
          <fs_entity2>-output_die = lv_output_die.
          IF ls_lot4-asy_facility IS NOT INITIAL.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = ls_lot4-asy_facility
              IMPORTING
                output = ls_lot4-asy_facility.
            SELECT SINGLE name1 FROM lfa1 INTO  lv_name1 WHERE lifnr EQ ls_lot4-asy_facility .
            IF sy-subrc EQ 0.
              <fs_entity2>-vendor = lv_name1.
            ENDIF.
          ELSE.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = ls_lot4-bump_facility
              IMPORTING
                output = ls_lot4-bump_facility.

            SELECT SINGLE name1 FROM lfa1 INTO lv_name1 WHERE lifnr EQ ls_lot4-bump_facility .
            IF sy-subrc EQ 0.
              <fs_entity2>-vendor = lv_name1.
            ENDIF.
          ENDIF.
        ENDIF.
        CASE flag.
          WHEN 'A'.
            DELETE lt_mat WHERE NOT mtart IN mtart_range.
          WHEN 'S'.
            DELETE lt_mat WHERE  mtart IN mtart_range.
          WHEN 'AS'.

        ENDCASE.

        " Get Material Status, BOM Status and Route Status
        IF asy_flag = 'X' OR scc_flag = 'X'.

          LOOP AT  lt_mat INTO ls_master4  WHERE  material_status =   'C' OR
*                                                                    material_status =   'W1' OR " JGAND37207
                                                                    material_status =   'W3' OR
                                                                    material_status =   'W4' OR
                                                                    material_status =   'X0'  .
            lv_ov_material_status  =  'R'   .
            IF lv_ov_material_status  IS NOT INITIAL .
              EXIT .
            ENDIF.
          ENDLOOP.
          IF lv_ov_material_status IS INITIAL .
            LOOP AT  lt_mat  INTO ls_master4  WHERE  material_status =   'P' OR
                                                       material_status =   'Q' OR
                                                       material_status =   'W2'  .
*              lv_ov_material_status  =  'R'   .
              lv_ov_material_status  =  'G'   .
              IF lv_ov_material_status  IS NOT INITIAL .
                EXIT .
              ENDIF.
            ENDLOOP.
            IF lv_ov_material_status IS INITIAL .
              LOOP AT  lt_mat  INTO ls_master4  WHERE    material_status =   'R1' OR
                                                         material_status =   'R2' OR
                                                         material_status =   'R' OR
                                                         material_status =   'W1'  .
                lv_ov_material_status  =  'G'   .
                IF lv_ov_material_status  IS NOT INITIAL .
                  EXIT .
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.

          " bom status
          LOOP AT  lt_mat  INTO ls_master4  WHERE  bom_status =   '01' OR
                                                           bom_status =   '04'   .
            lv_ov_bom_status  =  'R'   .
            IF lv_ov_bom_status  IS NOT INITIAL .
              EXIT .
            ENDIF.
          ENDLOOP.
          IF lv_ov_bom_status IS INITIAL .
            READ TABLE lt_mat   INTO ls_master4  WITH KEY  bom_status = '02'  .
            IF sy-subrc EQ 0 .
*              lv_ov_bom_status  =  'R'  .
              lv_ov_bom_status  =  'G'  .
            ELSE.
              READ TABLE lt_mat   INTO ls_master4  WITH KEY  bom_status = '03'  .
              IF sy-subrc EQ 0 .
                lv_ov_bom_status  =  'G'  .
              ENDIF.
            ENDIF.
          ENDIF.
          " Route status
          LOOP AT  lt_mat  INTO ls_master4  WHERE  route_status =   '01' OR route_status =   '1'  OR
                                                           route_status =   '05' OR route_status =   '5'  OR
            route_status =   '03'  OR route_status =   '3'   .
            lv_ov_route_status  =  'R'   .
            IF lv_ov_route_status  IS NOT INITIAL .
              EXIT .
            ENDIF.
          ENDLOOP.
          IF lv_ov_route_status IS INITIAL .
            READ TABLE lt_mat   INTO ls_master4  WITH KEY  route_status = '04'  .
            IF sy-subrc EQ 0 .
              lv_ov_route_status  =  'G'  .
            ENDIF.
          ENDIF.

          <fs_entity2>-material_status = lv_ov_material_status.
          <fs_entity2>-route_status = lv_ov_route_status.
          <fs_entity2>-bom_status = lv_ov_bom_status.
**********************************************************************
*          Added by Kiran
*          IF <fs_entity2>-npp_processor_name IS INITIAL.
*            <fs_entity2>-cenabled = abap_true.
*            <fs_entity2>-claimtxt = 'Claim'.
*          ELSE.
*            <fs_entity2>-cenabled = abap_false.
*            <fs_entity2>-claimtxt = 'Claimed'.
*          ENDIF.
**********************************************************************
        ENDIF.
        CLEAR: lv_ov_material_status,
               lv_ov_route_status,
               lv_ov_bom_status,
               lv_lot,
               lt_mat.

      ENDLOOP.

*      IF npp_flag = 'X'.
*        SORT et_entityset BY req_no DESCENDING npp_processor_name .
*      ELSE.
      SORT et_entityset BY req_no DESCENDING.
*      ENDIF.

    ENDIF.
*    CALL METHOD /iwbep/cl_mgw_data_util=>paging
*      EXPORTING
*        is_paging = is_paging
*      CHANGING
*        ct_data   = et_entityset.

*        ENDLOOP.

  ENDMETHOD.


  METHOD EBRHELPLINKSET_GET_ENTITY.
    DATA lo_exception          TYPE REF TO /iwbep/cx_mgw_tech_exception.

    SELECT SINGLE field1 INTO @DATA(lv_helplink) FROM zebr_config WHERE name EQ 'HELP_LINK'.
    IF sy-subrc IS INITIAL.
      er_entity-req_no = er_entity-req_no.
      er_entity-link   = lv_helplink.
      else.

      CREATE OBJECT lo_exception.
      IF  lo_exception IS BOUND.
        lo_exception->get_msg_container( )->add_message(
           EXPORTING
             iv_msg_type     = 'E'
             iv_msg_id      = '00'
             iv_msg_number  = '123'
             iv_msg_text    = 'No Help Link Maintained' ).
*          IV_MSG_V1
        RAISE EXCEPTION lo_exception.
        RETURN.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD EBRHELPLINKSET_GET_ENTITYSET.
    READ TABLE it_key_tab INTO DATA(ls_key) INDEX 1.
    IF sy-subrc IS INITIAL.
      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-req_no = ls_key-value.
    ENDIF.
  ENDMETHOD.


  METHOD EBRNOTIFSET_GET_ENTITY.
    DATA: ls_key_tab TYPE /iwbep/s_mgw_name_value_pair,
          lv_empid   TYPE zempid,
          lw_notif   TYPE zebr_t_notif_log,
          lv_req     TYPE zebr_de_req_no,
          lw_stat    TYPE zebr_t_stat_log.
    CONSTANTS:lc_req_02     TYPE char2 VALUE '02',
              lc_req_status TYPE char10 VALUE 'REQ_STATUS',
              lc_type       TYPE char10 VALUE 'DROP_DOWN',
              lc_n          TYPE char1 VALUE 'N'.

*    CONSTANTS: lc_notif TYPE char10 VALUE 'Notif'.
    READ TABLE it_key_tab WITH KEY name = 'ReqNo' INTO ls_key_tab.
    IF sy-subrc = 0.
      lv_req = ls_key_tab-value.
      er_entity-req_no   = lv_req.
    ENDIF.

    SELECT SINGLE * INTO @DATA(lw_header) FROM zebr_t_header  "#EC CI_ALL_FIELDS_NEEDED
      WHERE req_no EQ @lv_req.

    IF sy-subrc EQ 0.
      IF lw_header-npp_processor IS NOT INITIAL.
        SELECT SINGLE bname,accnt INTO @DATA(lw_usr02) FROM usr02 WHERE bname EQ @lw_header-npp_processor .
        IF sy-subrc EQ 0.
          SELECT SINGLE displayname INTO @DATA(lv_user) FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
          IF sy-subrc EQ 0.
            er_entity-username = lv_user.
            er_entity-flag = ''.
          ENDIF.
        ENDIF.
      ELSE.
        lw_header-npp_processor = sy-uname.
        lw_header-req_status_no = lc_req_02.
        SELECT SINGLE field2 INTO @DATA(lv_desc) FROM zebr_config WHERE
          name EQ @lc_req_status AND type EQ @lc_type AND field1 EQ @lc_req_02.
        IF sy-subrc EQ 0.
          lw_header-req_status = lv_desc.
        ENDIF.

        MODIFY zebr_t_header FROM lw_header .
        if sy-subrc eq 0 .
        COMMIT WORK.
        WAIT UP TO 1 SECONDS .
        endif.

        TRY.
        CALL METHOD zcl_ebr_utility=>email_functionality
          EXPORTING
            im_ebr      = lw_header-req_no
            im_req_type = lc_n
*           im_req_status =
            im_group    = 'CLAIM'
*          IMPORTING
*           et_notif    =
          .
         CATCH cx_abap_invalid_value .
        ENDTRY.

        lw_stat-req_no = lw_header-req_no.
        lw_stat-req_status_no = lc_req_02.
        lw_stat-req_status = lv_desc.

        lw_stat-created_on = sy-datum.
        lw_stat-created_at = sy-uzeit.
        lw_stat-created_by = sy-uname.

        INSERT zebr_t_stat_log FROM lw_stat.
        COMMIT WORK.

        SELECT SINGLE bname,accnt INTO @lw_usr02 FROM usr02 WHERE bname EQ @lw_header-npp_processor .
        IF sy-subrc EQ 0.
          SELECT SINGLE displayname INTO @lv_user FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
          IF sy-subrc EQ 0.
            er_entity-username = lv_user.
            er_entity-flag = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR:lw_stat,lw_header.
  ENDMETHOD.


  method EBRNOTIFSET_GET_ENTITYSET.

  endmethod.


  METHOD EBRNPPPLANNERSET_GET_ENTITYSET.

    TYPES : BEGIN OF ty_emp,
              bname TYPE xubname,
              empid TYPE zempid,
            END OF ty_emp.
    DATA: lt_account TYPE STANDARD TABLE OF ty_emp.
    SELECT DISTINCT npp_processor INTO TABLE @DATA(lt_npp) FROM zebr_t_header. "#EC CI_NOWHERE

    IF lt_npp IS NOT INITIAL.

      SELECT bname, accnt INTO TABLE @DATA(lt_usr02) FROM usr02 FOR ALL ENTRIES IN
        @lt_npp WHERE bname EQ @lt_npp-npp_processor.
      lt_account = lt_usr02.
      IF lt_account IS NOT INITIAL.
        SELECT employee_id,displayname INTO TABLE @DATA(lt_workday) FROM zworkday_data  "#EC CI_NO_TRANSFORM
          FOR ALL ENTRIES IN @lt_account WHERE employee_id EQ @lt_account-empid.
      ENDIF.
*    ENDIF.

      LOOP AT lt_npp INTO DATA(lw_user).
        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entity>).
        MOVE-CORRESPONDING lw_user TO <fs_entity>.
        READ TABLE lt_usr02 ASSIGNING FIELD-SYMBOL(<fs_usr02>) WITH KEY bname = lw_user-npp_processor.
        IF sy-subrc EQ 0.
          READ TABLE lt_workday ASSIGNING FIELD-SYMBOL(<fs_workday>) WITH KEY employee_id = <fs_usr02>-accnt.
          IF sy-subrc EQ 0.
            <fs_entity>-npp_name = <fs_workday>-displayname.
*            MOVE-CORRESPONDING <fs_workday> TO <fs_entity>.
          ENDIF.
        ENDIF.
      ENDLOOP.

      APPEND INITIAL LINE TO et_entityset ASSIGNING <fs_entity>.
    ENDIF.

    SORT et_entityset BY npp_name.
    REFRESH lt_account.
*        ENDIF.
  ENDMETHOD.


  METHOD EBRNPPSET_GET_ENTITY.

    DATA: ls_key_tab   TYPE /iwbep/s_mgw_name_value_pair,
          lv_empid     TYPE zempid,
          lw_notif     TYPE zebr_t_notif,
          lv_ebr       TYPE zebr_de_req_no,
*          lv_message   TYPE bapi_msg,
          lo_exception TYPE REF TO /iwbep/cx_mgw_tech_exception.

    CREATE OBJECT lo_exception.
*    CONSTANTS: lc_notif TYPE char10 VALUE 'Notif'.
    READ TABLE it_key_tab WITH KEY name = 'ReqNo' INTO ls_key_tab.
    IF sy-subrc = 0.
      lv_ebr = ls_key_tab-value.
    ENDIF.

    IF lv_ebr IS NOT INITIAL.
      DATA(lv_user) = sy-uname.

      SELECT SINGLE bname,accnt INTO @DATA(lw_usr02) FROM usr02 WHERE bname EQ @lv_user.
      IF sy-subrc EQ 0.
        SELECT SINGLE primary_email INTO @DATA(lv_email) FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
      ENDIF.
      SELECT SINGLE * INTO @DATA(lw_not) FROM zebr_t_notif WHERE req_no EQ @lv_ebr AND email EQ @lv_email.  "#EC CI_ALL_FIELDS_NEEDED
      IF sy-subrc EQ 0.
*        CONCATENATE TEXT-005  lv_ebr INTO lv_message SEPARATED BY space.
*        er_entity-message = lv_message.
        er_entity-flag = 'Y'.
      ELSE.
        SELECT SINGLE req_no,status INTO @DATA(lw_header) FROM zebr_t_header
          WHERE req_no EQ @lv_ebr.
        IF sy-subrc EQ 0.


          lw_notif-req_no = lw_header-req_no.
          lw_notif-email = lv_email.
*        lw_notif-type = text-003.
*        lw_notif-sentdate = sy-datum.
*        lw_notif-senttime = sy-uzeit.
*        lw_notif-req_status = lw_header-status.
          lw_notif-subscribe = 'X'.
          INSERT zebr_t_notif FROM lw_notif.
          COMMIT WORK.
          IF sy-subrc EQ 0.

*        IF sy-subrc EQ 0 AND lo_exception IS BOUND.
*            CONCATENATE TEXT-001 lw_header-req_no INTO lv_message SEPARATED BY space.
*          lo_exception->get_msg_container( )->add_message(
*             EXPORTING
*               iv_msg_type     = 'S'
*               iv_msg_id      = '00'
*               iv_msg_number  = '001'
*               iv_msg_text    = lv_message ).
**          IV_MSG_V1
*          RAISE EXCEPTION lo_exception. "STMNT_EXIT
*          RETURN.
*            er_entity-message = lv_message.
            er_entity-flag = 'X'.
*          ELSE.
*            CONCATENATE TEXT-002 '' lw_header-req_no INTO lv_message SEPARATED BY space.
*        lo_exception->get_msg_container( )->add_message(
*         EXPORTING
*           iv_msg_type     = 'I'
*           iv_msg_id      = '00'
*           iv_msg_number  = '002'
*           iv_msg_text    = lv_message ).
*        RAISE EXCEPTION lo_exception.
*        RETURN.
*            er_entity-message = lv_message.
*            er_entity-flag = ''.
*          CONCATENATE TEXT-001 lw_header-req_no INTO lv_message.
*          er_entity-message = lv_message .
*        ELSE.
*          CONCATENATE TEXT-002 lw_header-req_no INTO lv_message.
*          er_entity-message = lv_message.
          ENDIF.
        ENDIF.
*        CLEAR:lv_message.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD EBRNPPSET_GET_ENTITYSET.

*    DATA: lt_npp TYPE TABLE OF          selopt,
*          ls_npp TYPE  selopt.
*    IF it_filter_select_options IS NOT INITIAL.
*      LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options).
*        CASE ls_filter_select_options-property .
*          WHEN 'NppMail'.
*            LOOP AT ls_filter_select_options-select_options INTO DATA(ls_so) WHERE low IS NOT INITIAL.
*              MOVE-CORRESPONDING ls_so TO ls_npp.
*              INSERT ls_npp INTO TABLE lt_npp.
*              CLEAR ls_npp.
*            ENDLOOP.
*        ENDCASE.
*      ENDLOOP.
*    ENDIF.
*
*    IF lt_npp IS NOT INITIAL.
*      SELECT primary_email INTO TABLE et_entityset FROM zworkday_data
*        WHERE employee_id IN lt_npp.
*    ENDIF.
  ENDMETHOD.


  method EBRPACKAGESET_GET_ENTITY.

  endmethod.


  METHOD EBRPACKAGESET_GET_ENTITYSET.

*    SELECT DISTINCT package_type INTO TABLE et_entityset FROM zebr_t_header. "#EC CI_NOWHERE
*    SORT et_entityset BY package_type.
*    DELETE ADJACENT DUPLICATES FROM et_entityset COMPARING package_type.
*    DELETE et_entityset WHERE  package_type EQ ''.

  ENDMETHOD.


  METHOD EBRREQUESTTYPESE_GET_ENTITYSET.

    SELECT DISTINCT field1 field2 INTO CORRESPONDING FIELDS OF TABLE et_entityset FROM zebr_config
        WHERE name EQ 'REQ_TYPE' AND type EQ 'DROP_DOWN'.

    SORT et_entityset BY field1.

    DELETE et_entityset WHERE field1 EQ ''.

  ENDMETHOD.


  METHOD EBRROLESSET_GET_ENTITY.

    DATA(lv_user) = sy-uname.

    CALL FUNCTION 'AUTHORITY_CHECK'
      EXPORTING
*       new_buffering       = 3
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
    IF sy-subrc NE 2.
      er_entity-npp_flag = ''.
    ELSE.
      er_entity-npp_flag = 'X'.
      SELECT SINGLE bname,accnt INTO @DATA(lw_usr02) FROM usr02 WHERE bname EQ @lv_user.
      IF sy-subrc EQ 0.
        SELECT SINGLE displayname INTO @DATA(lv_name) FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
        IF sy-subrc EQ 0.
          er_entity-npp_name = lv_name.
          er_entity-npp_user = lv_user.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'AUTHORITY_CHECK'
      EXPORTING
*       new_buffering       = 3
        user                = sy-uname
        object              = 'ZEBR_OBJ'
        field1              = 'ZEBR_ROLE'
        value1              = 'ASY'
      EXCEPTIONS
        user_dont_exist     = 1
        user_is_authorized  = 2
        user_not_authorized = 3
        user_is_locked      = 4
        OTHERS              = 5.
    IF sy-subrc NE 2.
      er_entity-asy_flag = ''.
    ELSE.
      er_entity-asy_flag = 'X'.
      SELECT SINGLE bname,accnt INTO @lw_usr02 FROM usr02 WHERE bname EQ @lv_user.
      IF sy-subrc EQ 0.
        SELECT SINGLE displayname INTO @lv_name FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
        IF sy-subrc EQ 0.
          er_entity-asy_name = lv_name.
          er_entity-asy_user = lv_user.
        ENDIF.
      ENDIF.
    ENDIF.
    CALL FUNCTION 'AUTHORITY_CHECK'
      EXPORTING
*       new_buffering       = 3
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
    IF sy-subrc NE 2.
      er_entity-req_flag = ''.
    ELSE.
      er_entity-req_flag = 'X'.
      SELECT SINGLE bname,accnt INTO @lw_usr02 FROM usr02 WHERE bname EQ @lv_user.
      IF sy-subrc EQ 0.
        SELECT SINGLE displayname INTO @lv_name FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
        IF sy-subrc EQ 0.
          er_entity-req_name = lv_name.
          er_entity-req_user = lv_user.
        ENDIF.
      ENDIF.
    ENDIF.
    CALL FUNCTION 'AUTHORITY_CHECK'
      EXPORTING
*       new_buffering       = 3
        user                = sy-uname
        object              = 'ZEBR_OBJ'
        field1              = 'ZEBR_ROLE'
        value1              = 'SCC'
      EXCEPTIONS
        user_dont_exist     = 1
        user_is_authorized  = 2
        user_not_authorized = 3
        user_is_locked      = 4
        OTHERS              = 5.
    IF sy-subrc NE 2.
      er_entity-scc_flag = ''.
    ELSE.
      er_entity-scc_flag = 'X'.
      SELECT SINGLE bname,accnt INTO @lw_usr02 FROM usr02 WHERE bname EQ @lv_user.
      IF sy-subrc EQ 0.
        SELECT SINGLE displayname INTO @lv_name FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
        IF sy-subrc EQ 0.
          er_entity-scc_name = lv_name.
          er_entity-scc_user = lv_user.
        ENDIF.
      ENDIF.
    ENDIF.
        CALL FUNCTION 'AUTHORITY_CHECK'
      EXPORTING
*       new_buffering       = 3
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
    IF sy-subrc NE 2.
      er_entity-scm_flag = ''.
    ELSE.
      er_entity-scm_flag = 'X'.
      SELECT SINGLE bname,accnt INTO @lw_usr02 FROM usr02 WHERE bname EQ @lv_user.
      IF sy-subrc EQ 0.
        SELECT SINGLE displayname INTO @lv_name FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
        IF sy-subrc EQ 0.
          er_entity-scm_name = lv_name.
          er_entity-scm_user = lv_user.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD EBRROLESSET_GET_ENTITYSET.
*
*    DATA(lv_user) = sy-uname.
*
*    CALL FUNCTION 'AUTHORITY_CHECK'
*      EXPORTING
*        new_buffering = 3
*        user          = sy-uname
*        object        = 'ZEBR_OBJ'
*        field1        = 'ZEBR_ROLE'
*        value1        = 'NPP'
*  EXCEPTIONS
*       USER_DONT_EXIST           = 1
*       USER_IS_AUTHORIZED        = 2
*       USER_NOT_AUTHORIZED       = 3
*       USER_IS_LOCKED            = 4
*       OTHERS        = 5
*      .
*    IF sy-subrc <> 0.
*      et_entityset-npp_flag = ''.
*    ELSE.
*      et_entity-npp_flag = 'X'.
*      SELECT SINGLE bname,accnt INTO @DATA(lw_usr02) FROM usr02 WHERE bname EQ @lv_user.
*      IF sy-subrc EQ 0.
*        SELECT SINGLE displayname INTO @DATA(lv_name) FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
*        IF sy-subrc EQ 0.
*          er_entity-npp_name = lv_name.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    CALL FUNCTION 'AUTHORITY_CHECK'
*      EXPORTING
*        new_buffering = 3
*        user          = sy-uname
*        object        = 'ZEBR_OBJ'
*        field1        = 'ZEBR_ROLE'
*        value1        = 'ASY'
* EXCEPTIONS
*       USER_DONT_EXIST           = 1
*       USER_IS_AUTHORIZED        = 2
*       USER_NOT_AUTHORIZED       = 3
*       USER_IS_LOCKED            = 4
*       OTHERS        = 5
*      .
*    IF sy-subrc <> 0.
*      er_entity-asy_flag = ''.
*    ELSE.
*      er_entity-asy_flag = 'X'.
*      SELECT SINGLE bname,accnt INTO @lw_usr02 FROM usr02 WHERE bname EQ @lv_user.
*      IF sy-subrc EQ 0.
*        SELECT SINGLE displayname INTO @lv_name FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
*        IF sy-subrc EQ 0.
*          er_entity-asy_name = lv_name.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*    CALL FUNCTION 'AUTHORITY_CHECK'
*      EXPORTING
*        new_buffering = 3
*        user          = sy-uname
*        object        = 'ZEBR_OBJ'
*        field1        = 'ZEBR_ROLE'
*        value1        = 'REQ'
* EXCEPTIONS
*       USER_DONT_EXIST           = 1
*       USER_IS_AUTHORIZED        = 2
*       USER_NOT_AUTHORIZED       = 3
*       USER_IS_LOCKED            = 4
*       OTHERS        = 5
*      .
*    IF sy-subrc <> 0.
*      er_entity-req_flag = ''.
*    ELSE.
*      er_entity-req_flag = 'X'.
*      SELECT SINGLE bname,accnt INTO @lw_usr02 FROM usr02 WHERE bname EQ @lv_user.
*      IF sy-subrc EQ 0.
*        SELECT SINGLE displayname INTO @lv_name FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
*        IF sy-subrc EQ 0.
*          er_entity-req_name = lv_name.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*    CALL FUNCTION 'AUTHORITY_CHECK'
*      EXPORTING
*        new_buffering = 3
*        user          = sy-uname
*        object        = 'ZEBR_OBJ'
*        field1        = 'ZEBR_ROLE'
*        value1        = 'SCC'
* EXCEPTIONS
*       USER_DONT_EXIST           = 1
*       USER_IS_AUTHORIZED        = 2
*       USER_NOT_AUTHORIZED       = 3
*       USER_IS_LOCKED            = 4
*       OTHERS        = 5
*      .
*    IF sy-subrc <> 0.
*      er_entity-scc_flag = ''.
*    ELSE.
*      er_entity-scc_flag = 'X'.
*      SELECT SINGLE bname,accnt INTO @lw_usr02 FROM usr02 WHERE bname EQ @lv_user.
*      IF sy-subrc EQ 0.
*        SELECT SINGLE displayname INTO @lv_name FROM zworkday_data WHERE employee_id EQ @lw_usr02-accnt.
*        IF sy-subrc EQ 0.
*          er_entity-scc_name = lv_name.
*        ENDIF.
*      ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD EBRSTATUSSET_GET_ENTITYSET.
*    SELECT DISTINCT status FROM zebr_t_header INTO TABLE et_entityset.   "#EC CI_NOWHERE


    SELECT DISTINCT field1 field2 INTO CORRESPONDING FIELDS OF TABLE et_entityset FROM zebr_config
      WHERE name EQ 'REQ_STATUS' AND type EQ 'DROP_DOWN'.
     APPEND INITIAL LINE TO et_entityset .

    SORT et_entityset BY field1.

  ENDMETHOD.


  METHOD EBRVARIANTSET_CREATE_ENTITY.

    DATA: ls_variant TYPE zebr_t_variant.
    io_data_provider->read_entry_data( IMPORTING es_data = ls_variant ).

    IF ls_variant IS NOT INITIAL.
      ls_variant-zuser = sy-uname.
      SELECT SINGLE * INTO @DATA(ls_ebrvar) FROM zebr_t_variant    "#EC CI_ALL_FIELDS_NEEDED
        WHERE zuser EQ @ls_variant-zuser.

        MODIFY zebr_t_variant FROM ls_variant.
        COMMIT WORK.

      CLEAR ls_variant.
    ENDIF.

  ENDMETHOD.


  METHOD EBRVARIANTSET_GET_ENTITY.

    DATA: ls_key_tab TYPE /iwbep/s_mgw_name_value_pair,
          lv_user    TYPE zebr_de_created_by.

* Get the key property values
*    READ TABLE it_key_tab WITH KEY name = 'Zuser' INTO ls_key_tab.
*    IF sy-subrc = 0.
*      lv_user = ls_key_tab-value.
*    ENDIF.

    lv_user = sy-uname.

    IF lv_user IS NOT INITIAL.
      SELECT SINGLE * INTO @DATA(ls_variant) FROM zebr_t_variant
        WHERE zuser EQ @lv_user.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING ls_variant TO er_entity.
      ENDIF.
    ENDIF.
    CLEAR lv_user.
  ENDMETHOD.


  method EBRVARIANTSET_GET_ENTITYSET.


  endmethod.
ENDCLASS.
