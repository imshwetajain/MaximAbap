class ZCL_ZBSR_FORM_DPC_EXT_1 definition
  public
  inheriting from ZCL_ZBSR_FORM_DPC
  create public .

public section.

  class-methods READ_MAT_CHAR_IDENTIFIER
    importing
      !IV_MATNR type MATNR
    returning
      value(RS_CHAR) type BAPI1003_ALLOC_VALUES_CHAR .
  class-methods READ_TEXT_FORM
    importing
      !ID type THEAD-TDID
      !NAME type THEAD-TDNAME
      !OBJECT type THEAD-TDOBJECT
    returning
      value(TEXT) type STRING .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
    redefinition .
protected section.

  methods BSRAUTHSET_GET_ENTITY
    redefinition .
  methods BSRAUTHSET_GET_ENTITYSET
    redefinition .
  methods BSRDEFAULTSSET_GET_ENTITY
    redefinition .
  methods BSRDEFAULTSSET_GET_ENTITYSET
    redefinition .
  methods BSRDEPTSET_GET_ENTITYSET
    redefinition .
  methods BSRHEADERSET_CREATE_ENTITY
    redefinition .
*  methods BSRDEFAULTSSET_GET_ENTITYSET
*    redefinition .
*  methods RECORDTYPESET_GET_ENTITY
*    redefinition .
*  methods RECORDTYPESET_GET_ENTITYSET
*    redefinition .
*  methods BSRDEFAULTSSET_GET_ENTITY
*    redefinition .
  methods BSRHEADERSET_GET_ENTITY
    redefinition .
  methods BSRHEADERSET_GET_ENTITYSET
    redefinition .
  methods BSRITEMSET_GET_ENTITY
    redefinition .
  methods BSRITEMSET_GET_ENTITYSET
    redefinition .
  methods BSRMATSET_CREATE_ENTITY
    redefinition .
  methods BSRSTATUSLOGSET_GET_ENTITY
    redefinition .
  methods BSRSTATUSLOGSET_GET_ENTITYSET
    redefinition .
  methods BSRTESTMATSET_GET_ENTITYSET
    redefinition .
  methods DISCARDSET_CREATE_ENTITY
    redefinition .
  methods DISCARDSET_GET_ENTITY
    redefinition .
  methods DISCARDSET_GET_ENTITYSET
    redefinition .
  methods DISCARDSET_UPDATE_ENTITY
    redefinition .
  methods PLANTSHSET_GET_ENTITY
    redefinition .
  methods PLANTSHSET_GET_ENTITYSET
    redefinition .
  methods BSRCONFIGSET_GET_ENTITYSET
    redefinition .
private section.

  methods READ_TEXT
    importing
      !ID type THEAD-TDID
      !NAME type THEAD-TDNAME
      !OBJECT type THEAD-TDOBJECT
    returning
      value(TEXT) type STRING .
  methods SAVE_TEXT
    importing
      !ID type THEAD-TDID
      !NAME type THEAD-TDNAME
      !OBJECT type THEAD-TDOBJECT
      !TEXT type STRING
    returning
      value(EV_FAILED) type BOOLEAN .
ENDCLASS.



CLASS ZCL_ZBSR_FORM_DPC_EXT_1 IMPLEMENTATION.


  METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY.
    DATA: lr_deep_entity TYPE zcl_zbsr_form_mpc_ext=>ts_head_item,
          ls_header      TYPE zbsr_header,
          lt_item        TYPE TABLE OF zbsr_item,
          lt_notif       TYPE TABLE OF zbsr_notifcation,
          lv_reqid       TYPE zbsr_header-req_id,
          lv_reqid1      TYPE zbsr_header-req_id,
          lt_bsr_log     TYPE TABLE OF zbsr_status_log,
          ls_bsr_log     TYPE zbsr_status_log,
          lo_exception   TYPE REF TO /iwbep/cx_mgw_tech_exception,
          lv_num(5)      TYPE n,
          lv_name        TYPE thead-tdname,
          lv_cumqty      TYPE zbsr_item-qty,
          lv_matnr       TYPE matnr,
          ls_char        TYPE bapi1003_alloc_values_char.
    DATA: lt_stpo TYPE ztt_bom_com_level. "gg8
    CLEAR: lv_reqid,ls_header,lt_item,lr_deep_entity,lv_name ,lv_num,lv_cumqty,ls_char.
    CREATE OBJECT lo_exception.
    io_data_provider->read_entry_data(
   IMPORTING
   es_data = lr_deep_entity ).
    IF lr_deep_entity IS INITIAL AND lo_exception IS BOUND.
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

    IF NOT lr_deep_entity-req_id IS INITIAL.
      WRITE lr_deep_entity-req_id TO lv_reqid NO-ZERO.
      CONDENSE lv_reqid.
      IF lr_deep_entity-req_id NE '*'.
        SELECT SINGLE * FROM zbsr_header INTO @DATA(ls_header_db) WHERE req_id EQ @lv_reqid. "#EC CI_ALL_FIELDS_NEEDED
      ENDIF.

      MOVE-CORRESPONDING lr_deep_entity TO ls_header.
      IF NOT lr_deep_entity-specific_inst IS INITIAL.
        SELECT SINGLE * FROM zbucp_config INTO @DATA(ls_config) WHERE name EQ 'SPECIFIC_INSTRUCTION' AND type EQ 'DROP_DOWN' AND field1 EQ @lr_deep_entity-specific_inst. "#EC CI_ALL_FIELDS_NEEDED
        IF sy-subrc IS INITIAL AND ls_config-field3 EQ 1 AND lr_deep_entity-plnnr_alt IS INITIAL ."AND lr_deep_entity-tecn IS INITIAL.
          lo_exception->get_msg_container( )->add_message(
                     EXPORTING
                       iv_msg_type     = 'E'
                       iv_msg_id      = '00'
                       iv_msg_number  = '123'
                       iv_msg_text    = 'Route Name is mandatory' ).
          RAISE EXCEPTION lo_exception.
          RETURN.
        ENDIF.
      ENDIF.
      IF NOT lr_deep_entity-special_instr IS INITIAL.
        CLEAR ls_config.
        SELECT SINGLE * FROM zbucp_config INTO ls_config WHERE name EQ 'SPECIAL_INSTRUCTION' AND type EQ 'DROP_DOWN' AND field1 EQ lr_deep_entity-special_instr. "#EC CI_ALL_FIELDS_NEEDED
        IF sy-subrc IS INITIAL AND ls_config-field3 EQ 1 AND lr_deep_entity-destination IS INITIAL.
          lo_exception->get_msg_container( )->add_message(
                     EXPORTING
                       iv_msg_type     = 'E'
                       iv_msg_id      = '00'
                       iv_msg_number  = '123'
                       iv_msg_text    = 'Destination is mandatory' ).
          RAISE EXCEPTION lo_exception.
          RETURN.
        ENDIF.

        SELECT * FROM zbucp_config INTO TABLE @DATA(lt_config_spe) WHERE name EQ 'ZBSR_REL' AND field1 = @ls_config-field1. "#EC CI_ALL_FIELDS_NEEDED
        IF sy-subrc EQ 0.
          READ TABLE lt_config_spe TRANSPORTING NO FIELDS WITH KEY field3 = lr_deep_entity-wip_location.
          IF sy-subrc EQ 4.
            lo_exception->get_msg_container( )->add_message(
                   EXPORTING
                     iv_msg_type     = 'E'
                     iv_msg_id      = '00'
                     iv_msg_number  = '999'
                     iv_msg_text    = 'Wip location is not valid for Rel location' ).
            RAISE EXCEPTION lo_exception.
            RETURN.
          ENDIF.

          " VALIDATION FOR REL ID
          IF lr_deep_entity-relid IS NOT INITIAL .
            SELECT SINGLE  external_id    FROM /rpm/item_d   INTO @DATA(lv_projectid)  WHERE external_id  =  @lr_deep_entity-relid
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
            ELSE.
              ls_header-relid              = lr_deep_entity-relid.
            ENDIF.
          ELSE.
            lo_exception->get_msg_container( )->add_message(
               EXPORTING
                 iv_msg_type     = 'E'
                 iv_msg_id      = '00'
                 iv_msg_number  = '123'
                 iv_msg_text    = 'Rel id is mandatory for Rel Routes' ).
            RAISE EXCEPTION lo_exception.
            RETURN.
          ENDIF.

        ENDIF.

      ENDIF.
      IF NOT lr_deep_entity-output_material IS INITIAL.


        SELECT * FROM zbucp_config INTO TABLE lt_config_spe WHERE name EQ 'SPECIAL_INSTRUCTION' AND type = 'DROP_DOWN' AND field1 = lr_deep_entity-special_instr
                                                                                                 AND field4 = 'X'
                                                                                                AND field5 EQ 'BSR' "AND default_ind = ' '.
                                                                                                AND ( default_ind = ' ' OR default_ind = '1300' ). "#EC CI_ALL_FIELDS_NEEDED
        "strt gg8
*        IF lr_deep_entity-special_instr = '2' OR lr_deep_entity-special_instr = '1'.
        IF sy-subrc EQ 0.
          SELECT field1 FROM zbucp_config INTO TABLE @DATA(lt_speci_instr)  WHERE name EQ 'ZIGNORE_MAT_VALIDATION' AND type = 'DROP_DOWN'
                                                                       AND field1 = @lr_deep_entity-special_instr AND field2 = @lr_deep_entity-specific_inst
                                                                        AND field5 EQ 'BSR' AND default_ind = @lr_deep_entity-wip_location.
          IF sy-subrc NE 0.

            CLEAR lv_matnr.
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input  = lr_deep_entity-sap_material
              IMPORTING
                output = lv_matnr.

            CALL METHOD zcl_bom_explode_util=>bom_explode
              EXPORTING
                iv_matnr              = lv_matnr
                iv_werks              = lr_deep_entity-werks
                iv_get_marterial_type = 'X'
              CHANGING
                it_stpo               = lt_stpo.

            TRANSLATE lr_deep_entity-output_material TO UPPER CASE.
            CLEAR lv_matnr.
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input  = lr_deep_entity-output_material
              IMPORTING
                output = lv_matnr.

            READ TABLE lt_stpo ASSIGNING FIELD-SYMBOL(<fs>) WITH KEY matnr = lv_matnr mtart = 'ZTST'.
            IF sy-subrc EQ 0.
              ls_header-output_material = lv_matnr.
            ELSEIF sy-subrc EQ 4 AND lr_deep_entity-special_instr = '2'.
              lo_exception->get_msg_container( )->add_message(
                     EXPORTING
                       iv_msg_type     = 'E'
                       iv_msg_id      = '00'
                       iv_msg_number  = '123'
                       iv_msg_text    = 'No ZTST Material found. Resubmit as return to FT dispatch' ).
              RAISE EXCEPTION lo_exception.
              RETURN.
            ELSEIF  lr_deep_entity-special_instr = '1'.
              READ TABLE lt_stpo ASSIGNING FIELD-SYMBOL(<fs1>) WITH KEY matnr = lv_matnr mtart = 'FERT'.
              IF sy-subrc EQ 0.
                ls_header-output_material = lv_matnr.
              ELSE.
                lo_exception->get_msg_container( )->add_message(
                EXPORTING
                  iv_msg_type     = 'E'
                  iv_msg_id      = '00'
                  iv_msg_number  = '123'
                  iv_msg_text    = 'Not a valid FERT Material' ).
                RAISE EXCEPTION lo_exception.
                RETURN.
              ENDIF.
            ENDIF.
*         READ TABLE lt_stpo ASSIGNING FIELD-SYMBOL(<fs>) WITH KEY matnr = lv_matnr mtart = 'FERT'.
*          IF NOT line_exists( lt_stpo[  matnr = ls_header-output_material mtart = 'ZTST' ] ).
*            lo_exception->get_msg_container( )->add_message(
*                          EXPORTING
*                            iv_msg_type     = 'E'
*                            iv_msg_id      = '00'
*                            iv_msg_number  = '123'
*                            iv_msg_text    = 'No ZTST Material found. Resubmit as return to FT dispatch' ).
*            RAISE EXCEPTION lo_exception.
*            RETURN.
*          ENDIF.
          ELSE.
            "end gg8
            TRANSLATE lr_deep_entity-output_material TO UPPER CASE.
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input  = lr_deep_entity-output_material
              IMPORTING
                output = lv_matnr.
            ls_header-output_material = lv_matnr.
            SELECT SINGLE matnr,mtart FROM mara INTO @DATA(ls_mara_tmp) WHERE matnr EQ @lv_matnr.

            IF NOT sy-subrc IS INITIAL.
              lo_exception->get_msg_container( )->add_message(
                   EXPORTING
                     iv_msg_type     = 'E'
                     iv_msg_id      = '00'
                     iv_msg_number  = '123'
                     iv_msg_text    = 'Output Material' && ` ` && lr_deep_entity-output_material && ` ` && 'does not exist' ).
              RAISE EXCEPTION lo_exception.
              RETURN.
            ENDIF.
          ENDIF."gg8
        ENDIF.
      ENDIF.
      " convert group counter
      IF lr_deep_entity-plnal IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_header-plnal
          IMPORTING
            output = ls_header-plnal.
      ENDIF.
      " convert group counter
      IF NOT lr_deep_entity-sap_material IS INITIAL.
        TRANSLATE lr_deep_entity-sap_material TO UPPER CASE.
        CLEAR lv_matnr.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = lr_deep_entity-sap_material
          IMPORTING
            output = lv_matnr.
        ls_header-sap_material = lv_matnr.
        DATA(lv_matnr_uom)     = ls_header-sap_material.
        SELECT SINGLE matnr mtart FROM mara INTO ls_mara_tmp  WHERE matnr EQ lv_matnr.
        IF NOT sy-subrc IS INITIAL.
          lo_exception->get_msg_container( )->add_message(
               EXPORTING
                 iv_msg_type     = 'E'
                 iv_msg_id      = '00'
                 iv_msg_number  = '123'
                 iv_msg_text    = 'Input Material' && ` ` && lr_deep_entity-sap_material && ` ` && 'does not exist' ).
          RAISE EXCEPTION lo_exception.
          RETURN.

        ENDIF.
        ls_char = read_mat_char_identifier( iv_matnr = lv_matnr ).
        ls_header-product_line = ls_char-value_neutral.
        lr_deep_entity-product_line = ls_header-product_line.
        IF lr_deep_entity-uom IS INITIAL.
          SELECT SINGLE meins FROM mara INTO @DATA(lv_meins) WHERE matnr EQ @lv_matnr_uom.
          IF sy-subrc IS INITIAL.
            ls_header-uom = lv_meins.
            lr_deep_entity-uom = lv_meins.
          ENDIF.
        ENDIF.

        IF NOT lr_deep_entity-tecn IS INITIAL AND lr_deep_entity-tecn_no IS INITIAL.
          lo_exception->get_msg_container( )->add_message(
          EXPORTING
            iv_msg_type     = 'E'
            iv_msg_id      = '00'
            iv_msg_number  = '123'
            iv_msg_text    = 'TECN/SWR Number can be blank when TECN/SWR is checked' ).
          RAISE EXCEPTION lo_exception.
          RETURN.
        ENDIF.

        IF NOT lr_deep_entity-wip_location IS INITIAL.
          IF lr_deep_entity-wip_location EQ '1300'.
            IF ls_mara_tmp-mtart NE 'ZTST'.
              lo_exception->get_msg_container( )->add_message(
              EXPORTING
                iv_msg_type     = 'E'
                iv_msg_id      = '00'
                iv_msg_number  = '123'
                iv_msg_text    = 'Please enter a valid test material "ZTST" for WIP Location: "TCI" ' ).
              RAISE EXCEPTION lo_exception.
              RETURN.
            ENDIF.
          ELSEIF lr_deep_entity-wip_location EQ '1400'.
            IF ls_mara_tmp-mtart NE 'FERT'.
              lo_exception->get_msg_container( )->add_message(
            EXPORTING
              iv_msg_type     = 'E'
              iv_msg_id      = '00'
              iv_msg_number  = '123'
              iv_msg_text    = 'Please enter a valid test material "FERT" for WIP Location: "SAP/FG" ' ).
              RAISE EXCEPTION lo_exception.
              RETURN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      IF NOT lr_deep_entity-od_no IS INITIAL.
        UNPACK lr_deep_entity-od_no TO lr_deep_entity-od_no.
        SELECT SINGLE vbeln FROM lips INTO @DATA(lv_vbeln) WHERE vbeln EQ @lr_deep_entity-od_no AND matnr EQ @ls_header-sap_material .
        IF NOT sy-subrc IS INITIAL.
          lo_exception->get_msg_container( )->add_message(
          EXPORTING
            iv_msg_type     = 'E'
            iv_msg_id      = '00'
            iv_msg_number  = '123'
            iv_msg_text    = 'OD Number' && ` ` && lr_deep_entity-od_no && ` ` && 'doesnt exist in the system'  ).
          RAISE EXCEPTION lo_exception.
          RETURN.
        ENDIF.
      ENDIF.
      IF NOT lr_deep_entity-headitnav IS INITIAL AND lr_deep_entity-wip_location NE '1300'.
        LOOP AT lr_deep_entity-headitnav ASSIGNING FIELD-SYMBOL(<fs_unpack>).
          IF NOT <fs_unpack>-batch_no IS INITIAL.
            UNPACK <fs_unpack>-batch_no TO <fs_unpack>-batch_no.
          ENDIF.
        ENDLOOP.
        SELECT matnr, charg, clabs , cinsm , ceinm FROM mchb INTO TABLE @DATA(lt_mchb) FOR ALL ENTRIES IN @lr_deep_entity-headitnav "#EC CI_NO_TRANSFORM
                                                                           WHERE matnr EQ @ls_header-sap_material
                                                                             AND werks EQ @ls_header-werks
                                                                             AND lgort EQ @ls_header-wip_location
                                                                             AND charg EQ @lr_deep_entity-headitnav-batch_no
*                                                                             AND clabs GT 0.
                                                                             AND ( clabs GT 0 OR cinsm GT 0 OR ceinm GT 0 ).

        IF sy-subrc IS INITIAL.
          SELECT charg, licha FROM mch1 INTO TABLE @DATA(lt_mch1) FOR ALL ENTRIES IN @lt_mchb "#EC CI_NO_TRANSFORM
                                                                     WHERE matnr  EQ @lt_mchb-matnr
                                                                       AND charg  EQ @lt_mchb-charg.
        ENDIF.
      ENDIF.
      CLEAR lv_num.
      LOOP AT lr_deep_entity-headitnav ASSIGNING FIELD-SYMBOL(<fs_item>).
        DATA(lv_tabix) = sy-tabix.
        IF lr_deep_entity-wip_location NE '1300'.
          READ TABLE lt_mchb INTO DATA(ls_mchb) WITH KEY charg = <fs_item>-batch_no.
          IF sy-subrc IS INITIAL.
            READ TABLE lt_mch1 INTO DATA(ls_mch1) WITH KEY charg = <fs_item>-batch_no.
            IF sy-subrc IS INITIAL.
              <fs_item>-vendor_batch = ls_mch1-licha.
            ENDIF.
            DATA(lv_total) = ls_mchb-clabs + ls_mchb-ceinm + ls_mchb-cinsm.
*            IF <fs_item>-qty GT ls_mchb-clabs .
            IF <fs_item>-qty GT lv_total.
              lo_exception->get_msg_container( )->add_message(
               EXPORTING
                 iv_msg_type     = 'E'
                 iv_msg_id      = '00'
                 iv_msg_number  = '123'
                 iv_msg_text    = 'Quatity is greater than Batch' && ` ` && <fs_item>-batch_no && ` ` && 'quantity' ).
              RAISE EXCEPTION lo_exception.
            ENDIF.
            IF <fs_item>-qty IS INITIAL.
*              DATA(v_int) = trunc( ls_mchb-clabs ).
              DATA(v_int) = trunc( lv_total ).
              <fs_item>-qty = v_int.
            ENDIF.
            "gg8

*            SELECT SINGLE lgnum FROM t320 INTO @DATA(lv_lgnum)
*                         WHERE werks = @lr_deep_entity-werks
*                         AND lgort = @lr_deep_entity-wip_location.
*            SELECT lqnum,
*                     matnr,
*                     werks,
*                     charg,
*                     meins,
*                     verme,
*                     lgort FROM lqua
*                           INTO TABLE @DATA(lt_lqua)
*                          FOR ALL ENTRIES IN @lr_deep_entity-headitnav
*                           WHERE werks = @lr_deep_entity-werks
*                           AND lgort  = @lr_deep_entity-wip_location          "'1400'.
*                            AND lgnum = @lv_lgnum
*                           AND matnr = @lv_matnr
*                            AND charg EQ @lr_deep_entity-headitnav-batch_no
*                            AND bestq = ' '
*                            AND  verme > 0.
*            IF sy-subrc EQ 0.
*              SORT lt_lqua BY charg.
*              LOOP AT lr_deep_entity-headitnav ASSIGNING FIELD-SYMBOL(<fs_item11>).
*
*                READ TABLE lt_lqua ASSIGNING FIELD-SYMBOL(<fs_lqua>) WITH KEY charg = <fs_item11>-batch_no.
*                IF sy-subrc EQ 0.
*                  IF <fs_item11>-qty > <fs_lqua>-verme .
*                    DATA: fin_qty TYPE bstmg.
*                    WHILE <fs_item11>-qty NE fin_qty  .
*                      fin_qty = <fs_lqua>-verme + fin_qty.
*                      IF fin_qty >= <fs_item11>-qty .
*                        EXIT.
*                      ENDIF.
*                    ENDWHILE.
*                    IF <fs_item11>-qty NE fin_qty.
*                      lo_exception->get_msg_container( )->add_message(
*              EXPORTING
*                iv_msg_type     = 'E'
*                iv_msg_id      = '00'
*                iv_msg_number  = '123'
*               iv_msg_text    = 'Quantity entered is not in multiples available pallet size for this lot :' && <fs_item11>-vendor_batch ).
**                iv_msg_text    = 'Quatity is greater than Batch' && ` ` && <fs_item>-batch_no && ` ` && 'quantity' ).
*                      RAISE EXCEPTION lo_exception.
*
*                    ENDIF.
*                  ENDIF.
*                ENDIF.
*              ENDLOOP.
*            ENDIF.
            "gg8
          ELSE.
            lo_exception->get_msg_container( )->add_message(
            EXPORTING
              iv_msg_type     = 'E'
              iv_msg_id      = '00'
              iv_msg_number  = '123'
              iv_msg_text    = 'Batch' && ` ` && <fs_item>-batch_no && ` ` && 'doesnt exist' ).
            RAISE EXCEPTION lo_exception.
          ENDIF.
        ELSEIF lr_deep_entity-wip_location EQ '1300'.
          IF NOT <fs_item>-qty IS INITIAL.
            v_int = trunc( <fs_item>-qty ).
            <fs_item>-qty = v_int.
          ENDIF.
        ENDIF.
        IF lv_tabix EQ 1.
          lv_num = '00010'.
          WRITE lv_num TO <fs_item>-itm_no.
        ELSE.
          lv_num = lv_num + 10.
          WRITE lv_num TO <fs_item>-itm_no.
        ENDIF.

        <fs_item>-req_id = lv_reqid.
        <fs_item>-req_type = lr_deep_entity-req_type.
        IF <fs_item>-qty IS ASSIGNED.
          lv_cumqty = lv_cumqty + <fs_item>-qty.
        ENDIF.
        IF <fs_item> IS ASSIGNED.
          APPEND INITIAL LINE TO lt_item ASSIGNING FIELD-SYMBOL(<fs_item1>).
          MOVE-CORRESPONDING <fs_item> TO <fs_item1>.
          <fs_item1>-mandt = sy-mandt.
        ENDIF.
        CLEAR lv_tabix.
      ENDLOOP.
      "shweta open TO
      IF lr_deep_entity-headitnav[] IS NOT INITIAL.

        CLEAR lv_matnr.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = ls_header-sap_material
          IMPORTING
            output = lv_matnr.

        SELECT vbeln , charg FROM
            lips INTO TABLE @DATA(lt_vbeln)
            FOR ALL ENTRIES IN @lr_deep_entity-headitnav
            WHERE matnr EQ @lv_matnr
            AND charg EQ @lr_deep_entity-headitnav-batch_no.
        IF lt_vbeln[] IS NOT INITIAL.
          SORT  lt_vbeln[] BY vbeln.
          DELETE ADJACENT DUPLICATES FROM lt_vbeln COMPARING vbeln.
          SELECT vbeln
             FROM likp
             INTO TABLE @DATA(lt_bsr_likp)
             FOR ALL ENTRIES IN @lt_vbeln
             WHERE vbeln EQ @lt_vbeln-vbeln
             AND lfart EQ 'ZBSR'.
          IF lt_bsr_likp[] IS NOT INITIAL.
            SORT lt_bsr_likp BY vbeln.
            DELETE ADJACENT DUPLICATES FROM lt_bsr_likp COMPARING vbeln.
            SELECT tanum, charg, pquit,vbeln
                  FROM ltap
                  INTO TABLE @DATA(lt_ltap)
                  FOR ALL ENTRIES IN @lt_vbeln
                  WHERE matnr EQ @lv_matnr
*                  AND werks EQ @ls_header-werks
                  AND charg EQ @lt_vbeln-charg
                  AND vbeln eq @lt_vbeln-vbeln.
            IF lt_ltap IS NOT INITIAL.
              DELETE lt_ltap WHERE pquit ='X'.

              IF lines( lt_ltap ) > 0.
                data(lv_batch) = lt_ltap[ 1 ]-charg.
                lo_exception->get_msg_container( )->add_message(
                EXPORTING
                  iv_msg_type     = 'E'
                  iv_msg_id      = '00'
                  iv_msg_number  = '123'
                  iv_msg_text    = 'Batch' && ` ` && lv_batch && ` ` && 'is already used' ).
                RAISE EXCEPTION lo_exception.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.

      ENDIF.

      "shweta open TO
      LOOP AT lr_deep_entity-itnotifnav ASSIGNING FIELD-SYMBOL(<fs_notif>).
        IF sy-tabix EQ 1.
          lv_num = '00010'.
          WRITE lv_num TO <fs_notif>-itm_no.
        ELSE.
          lv_num  = lv_num  + 10.
          WRITE lv_num  TO <fs_notif>-itm_no.
        ENDIF.
        <fs_notif>-req_id = lv_reqid.
        IF <fs_notif> IS ASSIGNED.
          APPEND INITIAL LINE TO lt_notif ASSIGNING FIELD-SYMBOL(<fs_notif1>).
          MOVE-CORRESPONDING <fs_notif> TO <fs_notif1>.
          <fs_notif1>-mandt = sy-mandt.
        ENDIF.
      ENDLOOP.


      IF lr_deep_entity-req_id EQ '*'.
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = '01'
            object                  = 'ZBSR_NR'
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
        IF sy-subrc <> 0 AND lv_reqid IS INITIAL.
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
        lr_deep_entity-req_id = lv_reqid.
        ls_header-req_id = lv_reqid.
        DATA(lv_create)  = abap_true.
        LOOP AT lt_item ASSIGNING <fs_item1>.
          <fs_item1>-req_id = lv_reqid.
        ENDLOOP.
        LOOP AT lt_notif ASSIGNING <fs_notif1>.
          <fs_notif1>-req_id = lv_reqid.
        ENDLOOP.
      ENDIF.
      IF NOT lv_create IS INITIAL.
        lr_deep_entity-created_by    = sy-uname.
        lr_deep_entity-created_on    = sy-datum.
        ls_header-created_on         = sy-datum.
        ls_header-created_by         = sy-uname.
        lr_deep_entity-changed_by    = sy-uname.
        lr_deep_entity-changed_on    = sy-datum.
        ls_header-changed_on         = sy-datum.
        ls_header-changed_by         = sy-uname.
      ELSE.
        lr_deep_entity-changed_by    = sy-uname.
        lr_deep_entity-changed_on    = sy-datum.
        ls_header-changed_on         = sy-datum.
        ls_header-changed_by         = sy-uname.
        lr_deep_entity-created_by    = ls_header_db-created_by.
        lr_deep_entity-created_on    = ls_header_db-created_on .
        ls_header-created_on         = ls_header_db-created_on .
        ls_header-created_by         = ls_header_db-created_by.
      ENDIF.

      IF NOT lr_deep_entity-created_by IS INITIAL.
        SELECT SINGLE accnt FROM usr02 INTO @DATA(lv_accnt) WHERE bname EQ @lr_deep_entity-created_by.
        IF sy-subrc IS INITIAL.
          SELECT SINGLE sapusername, displayname, managerid FROM zworkday_data INTO @DATA(ls_workday) WHERE employee_id EQ @lv_accnt.
          IF sy-subrc IS INITIAL.
            lr_deep_entity-createdonuser = ls_workday-displayname.
            SELECT SINGLE sapusername displayname managerid FROM zworkday_data INTO ls_workday WHERE employee_id EQ ls_workday-managerid.
            IF sy-subrc IS INITIAL.
              lr_deep_entity-manager = ls_workday-displayname.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      IF NOT lr_deep_entity-changed_by IS INITIAL.
        SELECT SINGLE accnt FROM usr02 INTO lv_accnt WHERE bname EQ sy-uname.
        IF sy-subrc IS INITIAL.
          SELECT SINGLE sapusername displayname managerid FROM zworkday_data INTO ls_workday WHERE employee_id EQ lv_accnt.
          IF sy-subrc IS INITIAL.
            ls_header-changed_on         = sy-datum.
            lr_deep_entity-changed_on    = sy-datum.
            lr_deep_entity-changedonuser = ls_workday-displayname.
          ENDIF.
        ENDIF.
      ENDIF.
      DATA(v_int1) = trunc( lv_cumqty ).
      IF NOT v_int1 IS INITIAL.
        CLEAR lv_cumqty.
        lv_cumqty = v_int1.
      ENDIF.


      lr_deep_entity-qty           = lv_cumqty.
      ls_header-qty                = lv_cumqty.
      MODIFY zbsr_header FROM ls_header.
      IF NOT lv_reqid IS INITIAL.
        DELETE FROM zbsr_item WHERE req_id EQ lv_reqid.
        IF sy-subrc IS INITIAL.
          COMMIT WORK.
          DATA(lv_success) = abap_true.
        ENDIF.
        DELETE FROM zbsr_notifcation WHERE req_id EQ lv_reqid.
        IF sy-subrc IS INITIAL.
          COMMIT WORK.
          lv_success = abap_true.
        ENDIF.
        IF NOT lt_item IS INITIAL.
          MODIFY zbsr_item FROM TABLE lt_item.
        ENDIF.
        IF NOT lt_notif IS INITIAL.
          MODIFY zbsr_notifcation FROM TABLE lt_notif.
        ENDIF.
      ENDIF.
      IF NOT lv_reqid IS INITIAL .
        IF NOT lv_create IS INITIAL AND lr_deep_entity-req_status_sub IS INITIAL.
          ls_bsr_log-req_id     = lv_reqid.
          ls_bsr_log-new_status = 0.
          GET TIME STAMP FIELD ls_bsr_log-zwhen.
          ls_bsr_log-zwho       = sy-uname.
          IF NOT ls_bsr_log IS INITIAL.
            MODIFY zbsr_status_log FROM ls_bsr_log.
          ENDIF.
        ELSEIF NOT lr_deep_entity-req_status_sub IS INITIAL.
          SELECT * FROM zbsr_status_log INTO TABLE lt_bsr_log WHERE req_id EQ lv_reqid.
          IF sy-subrc IS INITIAL.
            SORT lt_bsr_log BY zwhen DESCENDING.
            READ TABLE lt_bsr_log INTO ls_bsr_log INDEX 1.
            ls_bsr_log-req_id     = lv_reqid.
            ls_bsr_log-old_status = ls_bsr_log-new_status.
            ls_bsr_log-new_status = lr_deep_entity-req_status_sub.
            GET TIME STAMP FIELD ls_bsr_log-zwhen.
            ls_bsr_log-zwho       = sy-uname.
            IF NOT ls_bsr_log IS INITIAL.
              MODIFY zbsr_status_log FROM ls_bsr_log.
            ENDIF.
          ELSEIF lr_deep_entity-req_status_sub EQ 1.
            ls_bsr_log-req_id     = lv_reqid.
            ls_bsr_log-new_status = 1.
            GET TIME STAMP FIELD ls_bsr_log-zwhen.
            ls_bsr_log-zwho       = sy-uname.
            IF NOT ls_bsr_log IS INITIAL.
              MODIFY zbsr_status_log FROM ls_bsr_log.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      IF NOT lr_deep_entity-req_status_sub IS INITIAL.
        DATA lt_mailto TYPE bcsy_smtpa.
        LOOP AT lt_notif ASSIGNING <fs_notif>.
          APPEND INITIAL LINE TO lt_mailto ASSIGNING FIELD-SYMBOL(<fs_email>).
          <fs_email> = <fs_notif>-email.
        ENDLOOP.

        zcl_bsr_app_utility=>notifications(
          EXPORTING
            p_req_id     = lv_reqid    " Request ID
            p_req_status = lr_deep_entity-req_status_sub
            t_mail_to    = lt_mailto ).   " Request Status
      ENDIF.
    ENDIF.
    copy_data_to_ref(
         EXPORTING
         is_data = lr_deep_entity
         CHANGING
         cr_data = er_deep_entity ).

    IF lv_reqid IS INITIAL .
      lv_reqid = lr_deep_entity-req_id.
    ENDIF.
    CHECK NOT lv_reqid IS INITIAL AND lv_reqid NE '*'.
    lv_name = sy-mandt && lv_reqid.
    IF NOT lr_deep_entity-addtext IS INITIAL.
      save_text(
        EXPORTING
          id        = zcl_zbsr_form_mpc_ext=>gc_tdid_at    " Text ID
          name      = lv_name    " Name
          object    = zcl_zbsr_form_mpc_ext=>gc_object    " Texts: application object
          text      = lr_deep_entity-addtext ).
    ENDIF.
    IF NOT lr_deep_entity-destination IS INITIAL.
      save_text(
         EXPORTING
           id        = zcl_zbsr_form_mpc_ext=>gc_tdid_des    " Text ID
           name      = lv_name    " Name
           object    = zcl_zbsr_form_mpc_ext=>gc_object    " Texts: application object
           text      = lr_deep_entity-destination ).
    ENDIF.
    IF NOT lr_deep_entity-location IS INITIAL.
      save_text(
   EXPORTING
     id        = zcl_zbsr_form_mpc_ext=>gc_tdid_loc    " Text ID
     name      = lv_name    " Name
     object    = zcl_zbsr_form_mpc_ext=>gc_object    " Texts: application object
     text      = lr_deep_entity-location ).
    ENDIF.
    IF NOT lr_deep_entity-reasforincomplete IS INITIAL.
      save_text(
   EXPORTING
     id        = zcl_zbsr_form_mpc_ext=>gc_tdid_inc    " Text ID
     name      = lv_name    " Name
     object    = zcl_zbsr_form_mpc_ext=>gc_object    " Texts: application object
     text      = lr_deep_entity-reasforincomplete ).
    ENDIF.
    IF NOT lr_deep_entity-purpose IS INITIAL.
      save_text(
     EXPORTING
       id        = zcl_zbsr_form_mpc_ext=>gc_tdid_res    " Text ID
       name      = lv_name    " Name
       object    = zcl_zbsr_form_mpc_ext=>gc_object    " Texts: application object
       text      = lr_deep_entity-purpose ).
    ENDIF.


  ENDMETHOD.


  METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY.
    DATA lo_exception   TYPE REF TO /iwbep/cx_mgw_tech_exception.
    CREATE OBJECT lo_exception.
    IF iv_entity_name EQ 'Discard'.
      DATA ls_dis TYPE zcl_zbsr_form_mpc_ext=>ts_discard.
      READ TABLE it_key_tab INTO DATA(ls_key) INDEX  1.
      IF sy-subrc IS INITIAL.
        ls_dis-req_id = ls_key-value.
        copy_data_to_ref(
         EXPORTING
         is_data = ls_dis
         CHANGING
         cr_data = er_entity ).
      ENDIF.
    ELSEIF iv_entity_name EQ 'BSRStatusLog'.
      DATA ls_statuslog TYPE zcl_zbsr_form_mpc_ext=>ts_bsrstatuslog.
      READ TABLE it_key_tab INTO DATA(ls_key1) INDEX 1.
      CHECK sy-subrc IS INITIAL.
      SELECT SINGLE * FROM zbsr_status_log INTO CORRESPONDING FIELDS OF ls_statuslog WHERE req_id EQ ls_key1-value.
      copy_data_to_ref(
      EXPORTING
        is_data = ls_statuslog
        CHANGING
        cr_data = er_entity ).
    ELSEIF iv_entity_name EQ 'BSRMat'.
      DATA ls_mat TYPE  zcl_zbsr_form_mpc_ext=>ts_bsrmat.
      copy_data_to_ref(
    EXPORTING
      is_data = ls_mat
       CHANGING
      cr_data = er_entity ).
    ELSEIF iv_entity_name EQ 'BSRAuth'.
      DATA ls_auth TYPE  zcl_zbsr_form_mpc_ext=>ts_bsrauth.
      DATA lv_reqid TYPE c LENGTH 12.
      DATA lv_role TYPE c LENGTH 10.

      CHECK NOT it_key_tab[] IS INITIAL.
      READ TABLE it_key_tab INTO DATA(ls_key2) WITH KEY name = 'ReqId'.
      IF sy-subrc IS INITIAL.
        lv_reqid = ls_key2-value.
      ENDIF.
      CLEAR ls_key1.
      READ TABLE it_key_tab INTO ls_key2 WITH KEY name = 'Auth'.
      IF sy-subrc IS INITIAL.
        lv_role = ls_key2-value.
      ENDIF.
      SELECT SINGLE req_status FROM zbsr_header INTO @DATA(lv_status) WHERE req_id EQ @lv_reqid.
      IF sy-subrc IS INITIAL.
        CASE lv_status.
          WHEN '0'.
            lv_role = 'REQ'.
          WHEN '1'.
            lv_role = 'SHIP'.
          WHEN '2' OR '99'.
            lv_role = 'DISP'.
        ENDCASE.
      ELSE.
        lv_role = 'REQ'.
      ENDIF.

      CHECK NOT lv_role IS INITIAL.
      AUTHORITY-CHECK OBJECT 'ZBSR_FORM' ID 'BSR_AUTH' FIELD lv_role.
      IF sy-subrc IS INITIAL.
        ls_auth-success = 'Y'.
*        IF lv_status LE 2.
*          CALL FUNCTION 'ENQUEUE_EZBSR_REQ_ID'
*            EXPORTING
*              mode_zbsr_header = 'E'
*              req_id           = lv_reqid
*            EXCEPTIONS
*              foreign_lock     = 1
*              system_failure   = 2
*              OTHERS           = 3.
*          IF sy-subrc <> 0.
*            IF lo_exception IS BOUND.
*              DATA lv_msg TYPE bapi_msg .
*              CALL FUNCTION 'FORMAT_MESSAGE'
*                EXPORTING
*                  id        = sy-msgid
*                  lang      = 'EN'
*                  no        = sy-msgno
*                  v1        = sy-msgv1
*                  v2        = sy-msgv2
*                  v3        = sy-msgv3
*                  v4        = sy-msgv4
*                IMPORTING
*                  msg       = lv_msg
*                EXCEPTIONS
*                  not_found = 1
*                  OTHERS    = 2.
*              IF sy-subrc <> 0.
** Implement suitable error handling here
*              ENDIF.
*              lo_exception->get_msg_container( )->add_message(
*                       EXPORTING
*                         iv_msg_type     = sy-msgty
*                         iv_msg_id      = sy-msgid
*                         iv_msg_number  = sy-msgno
*                         iv_msg_text    = lv_msg ).
**          IV_MSG_V1
*              RAISE EXCEPTION lo_exception.
*            ENDIF.
*          ENDIF.
*        ENDIF.
      ELSE.
        ls_auth-success = 'N'.
      ENDIF.
      ls_auth-auth    = lv_role.
      ls_auth-req_id  = lv_reqid.

      copy_data_to_ref(
      EXPORTING
        is_data = ls_auth
        CHANGING
        cr_data = er_entity ).
    ELSEIF iv_entity_name EQ 'BSRBatchSH'.
      DATA ls_batch TYPE  zcl_zbsr_form_mpc_ext=>ts_bsrbatchsh.
      copy_data_to_ref(
   EXPORTING
     is_data = ls_batch
     CHANGING
     cr_data = er_entity ).
    ENDIF.
  ENDMETHOD.


  METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET.

    DATA lt_default_cc TYPE TABLE OF zbsr_s_cc_defaults.
    DATA lt_default_dpt TYPE TABLE OF zbsr_s_dept_defaults.
    DATA lt_default_pl TYPE TABLE OF zbsr_s_prodl_defaults.
    DATA lt_default_rt TYPE TABLE OF zbsr_s_req_defaults.
    DATA lt_default_des TYPE TABLE OF zbsr_s_destination_defaults.
    DATA lt_default_rt_status TYPE TABLE OF zbsr_s_req_status_defaults.
    DATA lt_default_si TYPE TABLE OF zcl_zbsr_form_mpc_ext=>ts_bsrsplinstr.
    DATA lt_default_sp TYPE TABLE OF zcl_zbsr_form_mpc_ext=>ts_bsrspecinstr.
    DATA lt_default_una TYPE TABLE OF zbsr_s_units_defaults.
    DATA lt_default_wip TYPE TABLE OF zbsr_s_wip_defaults.
    DATA lt_default_bsrtype TYPE TABLE OF zbsr_s_bsr_type_defaults.
    DATA lt_default_comp_status TYPE TABLE OF  zbsr_s_compl_status_defaults .
    DATA lt_default_dept TYPE TABLE OF zbsr_s_dept_defaults.
    DATA lt_default_plant TYPE TABLE OF zbsr_s_plant_defaults.
    DATA ls_default TYPE zbsr_s_defaults.
    DATA: lv_ip_matnr       TYPE mara-matnr,
          lv_op_matnr       TYPE mara-matnr,
          lv_werks          TYPE marc-werks,
          lv_mfrpn_inp      TYPE mfrpn,
          lv_mfrpn_out_temp TYPE mfrpn,
          lv_mfrpn_out      TYPE mfrpn.
    DATA lv_reqid   TYPE zbsr_header-req_id.
    DATA lv_role    TYPE char10.
    DATA lv_counter TYPE char10.
    DATA lt_routing TYPE TABLE OF zbsr_s_route_defaults.
    DATA lt_filters       TYPE  /iwbep/t_mgw_select_option.
    DATA(ls_stpo) = VALUE zcl_bom_explode_util=>ts_stpo_level( ).
    DATA: lt_stpo TYPE ztt_bom_com_level.
    DATA lt_matcat TYPE TABLE OF cscmat.
    DATA lt_test_mat TYPE TABLE OF zcl_zbsr_form_mpc_ext=>ts_bsrtestmat.
    DATA lt_statuslog TYPE zcl_zbsr_form_mpc_ext=>tt_bsrstatuslog.
    DATA: ra_charg     TYPE RANGE OF charg_d,
          ra_licha     TYPE RANGE OF lichn,
          ra_werks     TYPE RANGE OF werks_d,
          ra_mfrpn     TYPE RANGE OF mfrpn,
          ra_lgort     TYPE RANGE OF lgort_d,
          ra_matnr     TYPE RANGE OF matnr,
          ra_emp       TYPE RANGE OF zempid,
          lt_batch     TYPE  zcl_zbsr_form_mpc_ext=>tt_bsrbatchsh,
          lo_exception TYPE REF TO /iwbep/cx_mgw_tech_exception.
    CREATE OBJECT lo_exception.
    CASE iv_entity_name .
      WHEN 'BSRConfig'.

        SELECT * FROM zbucp_config INTO  TABLE @DATA(it_zbucp_config) WHERE name EQ 'SPECIAL_INSTRUCTION' OR name EQ 'SPECIFIC_INSTRUCTION' OR name EQ 'ZBSR_HIDE_INST'.

        copy_data_to_ref(
        EXPORTING
        is_data = it_zbucp_config
        CHANGING
        cr_data = er_entityset ).

      WHEN 'BSRTestMat'.
        lt_filters = io_tech_request_context->get_filter( )->get_filter_select_options( ).
        LOOP AT lt_filters INTO DATA(ls_filters).
          READ TABLE ls_filters-select_options INTO DATA(ls_sel1) INDEX 1.
          IF ls_filters-property = 'MFRPN'.
            lv_mfrpn_inp = ls_sel1-low.
          ENDIF.
          IF ls_filters-property = 'WERKS'.
            lv_werks = ls_sel1-low.
          ENDIF.
          IF ls_filters-property  = 'SPECIAL_INSTR'.
            DATA(lv_speins) = ls_sel1-low.
          ENDIF.
          "gg8
*          IF ls_filters-property = 'MFRPN_OUT'.
*            lv_mfrpn_out = ls_sel1-low.
*          ENDIF.
          "gg8
          CLEAR ls_filters.
        ENDLOOP.
        SELECT * FROM zbucp_config INTO TABLE @DATA(lt_config_spe) WHERE name EQ 'SPECIAL_INSTRUCTION' AND type = 'DROP_DOWN' AND field1 = @lv_speins AND field4 = 'X'
                                                                                              AND field5 EQ 'BSR'" AND default_ind = ' '.
                                                                                              AND ( default_ind = ' ' OR default_ind = '1300' ). "#EC CI_ALL_FIELDS_NEEDED

*        READ TABLE lt_config_spe INTO DATA(ls_config_spe) WITH KEY field1 = lv_speins.
*        IF lv_mfrpn_out IS INITIAL. "gg8
        IF sy-subrc EQ 0 AND lv_mfrpn_inp IS NOT INITIAL . "gg8

          CLEAR lv_ip_matnr. "gg8
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input  = lv_mfrpn_inp
            IMPORTING
              output = lv_ip_matnr.


          CALL METHOD zcl_bom_explode_util=>bom_explode
            EXPORTING
              iv_matnr              = lv_ip_matnr
              iv_werks              = lv_werks
              iv_get_marterial_type = 'X'
            CHANGING
              it_stpo               = lt_stpo.

          IF sy-subrc EQ 0.
            DELETE lt_stpo WHERE mtart NE 'ZTST'.
            IF sy-subrc EQ 0 AND lines( lt_stpo ) > 0.
              TRY .
                  DATA(ls_stpo_t) = lt_stpo[ 1 ].
                  MOVE-CORRESPONDING ls_stpo_t TO ls_stpo.
                  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
                    EXPORTING
                      input  = ls_stpo-matnr
                    IMPORTING
                      output = lv_mfrpn_out_temp.

                  APPEND INITIAL LINE TO lt_test_mat ASSIGNING FIELD-SYMBOL(<fs_tm>).
                  <fs_tm>-mfrpn = lv_mfrpn_inp.
                  <fs_tm>-werks = lv_werks.
                  <fs_tm>-mfrpn_out = lv_mfrpn_out_temp.

                CATCH cx_sy_itab_line_not_found .

              ENDTRY.
            ENDIF.


          ENDIF.
          " gg8 scenario output material validation.
*        ELSE.
**
*          CLEAR lv_ip_matnr.
*          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
*            EXPORTING
*              input  = lv_mfrpn_inp
*            IMPORTING
*              output = lv_ip_matnr.
*
*          CLEAR lv_op_matnr.
*          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
*            EXPORTING
*              input  = lv_mfrpn_out
*            IMPORTING
*              output = lv_op_matnr.
*
*          DATA:lt_str     TYPE STANDARD TABLE OF stpov,
*               lt_equicat TYPE STANDARD TABLE OF cscequi,
*               lt_kndcat  TYPE STANDARD TABLE OF cscknd,
*               lt_matcat1 TYPE STANDARD TABLE OF cscmat,
*               lt_stdcat  TYPE STANDARD TABLE OF cscstd,
*               lt_tplcat  TYPE STANDARD TABLE OF csctpl.
*
*          CALL FUNCTION 'CS_WHERE_USED_MAT'
*            EXPORTING
*              datub                      = sy-datum
*              datuv                      = sy-datum
*              matnr                      = lv_op_matnr
*            TABLES
*              wultb                      = lt_str
*              equicat                    = lt_equicat
*              kndcat                     = lt_kndcat
*              matcat                     = lt_matcat1
*              stdcat                     = lt_stdcat
*              tplcat                     = lt_tplcat
**             PRJCAT                     =
*            EXCEPTIONS
*              call_invalid               = 1
*              material_not_found         = 2
*              no_where_used_rec_found    = 3
*              no_where_used_rec_selected = 4
*              no_where_used_rec_valid    = 5
*              OTHERS                     = 6.
**          IF sy-subrc EQ 0.
*** Implement suitable error handling here
*            READ TABLE lt_str ASSIGNING FIELD-SYMBOL(<fs11>) WITH KEY matnr = lv_ip_matnr.
**            IF sy-subrc EQ 0.
**
**            ENDIF.
**          ENDIF.
          " gg8 scenario output material validation.

        ENDIF.
        copy_data_to_ref(
       EXPORTING
       is_data = lt_test_mat
       CHANGING
       cr_data = er_entityset ).

      WHEN 'BSRDept'.
        SELECT * FROM zbucp_config INTO TABLE @DATA(lt_config) WHERE name EQ 'DEPARTMENT' AND type = 'DROP_DOWN' AND field5 EQ 'BSR' AND default_ind = ' '.
        CHECK sy-subrc IS INITIAL.
        LOOP AT lt_config ASSIGNING FIELD-SYMBOL(<fs1>).
          APPEND INITIAL LINE TO lt_default_dept ASSIGNING FIELD-SYMBOL(<fs>).
          <fs>-department     = <fs1>-field1.
          <fs>-department_txt = <fs1>-field2.
        ENDLOOP.
        copy_data_to_ref(
        EXPORTING
        is_data = lt_default_dept
        CHANGING
        cr_data = er_entityset ).

      WHEN 'BSRCC'.                       " cost center
        REFRESH lt_config.
        SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'COST CENTER' AND type = 'DROP_DOWN' AND field5 EQ 'BSR' AND default_ind = ' '.
        CHECK sy-subrc IS INITIAL.
        LOOP AT lt_config ASSIGNING <fs1>.
          APPEND INITIAL LINE TO lt_default_cc ASSIGNING FIELD-SYMBOL(<fs_cc>).
          <fs_cc>-cost_center = <fs1>-field1.
          <fs_cc>-cost_center_txt = <fs1>-field2.
        ENDLOOP.
        copy_data_to_ref(
       EXPORTING
       is_data = lt_default_cc
       CHANGING
       cr_data = er_entityset ).

      WHEN 'BSRComplSt'.                  " complted status
        REFRESH lt_config.
        SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'COMPLETION_STATUS' AND type = 'DROP_DOWN' AND field5 EQ 'BSR' AND default_ind = ' '.
        CHECK sy-subrc IS INITIAL.
        LOOP AT lt_config ASSIGNING <fs1>.
          APPEND INITIAL LINE TO lt_default_comp_status ASSIGNING FIELD-SYMBOL(<fs_cs>).
          <fs_cs>-completion_status = <fs1>-field1.
          <fs_cs>-completion_status_txt = <fs1>-field2.
        ENDLOOP.
        copy_data_to_ref(      EXPORTING
                                is_data = lt_default_comp_status
                                CHANGING
                                cr_data = er_entityset ).

      WHEN 'BSRReq'.                      " request type
        REFRESH lt_config.
        SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'REQUEST_TYPE' AND type = 'DROP_DOWN' AND field5 EQ 'BSR' AND default_ind = ' '.
        CHECK sy-subrc IS INITIAL.
        LOOP AT lt_config ASSIGNING <fs1> .
          APPEND INITIAL LINE TO lt_default_rt ASSIGNING FIELD-SYMBOL(<fs_rt>).
          <fs_rt>-req_type = <fs1>-field1.
          <fs_rt>-req_type_txt = <fs1>-field2.
        ENDLOOP.
        copy_data_to_ref(
       EXPORTING
       is_data = lt_default_rt
       CHANGING
       cr_data = er_entityset ).

      WHEN 'BSRDestination'.
        lt_filters = io_tech_request_context->get_filter( )->get_filter_select_options( ).
        IF lt_filters[] IS NOT INITIAL.
          REFRESH lt_config_spe[].
          SELECT * FROM zbucp_config INTO TABLE lt_config_spe WHERE name EQ 'SPECIAL_INSTRUCTION' AND type = 'DROP_DOWN' AND field5 EQ 'BSR'
                                                                                                            AND ( default_ind = ' ' OR default_ind = '1300' ). "#EC CI_ALL_FIELDS_NEEDED
          CHECK sy-subrc IS INITIAL.
          READ TABLE lt_filters INTO DATA(ls_filter1) INDEX 1.
          READ TABLE ls_filter1-select_options INTO DATA(ls_sel_1) INDEX 1.

          READ TABLE lt_config_spe INTO DATA(ls_config_spe) WITH KEY field1 = ls_sel_1-low.
          REFRESH lt_config.
          SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ zcl_bsr_app_utility=>lc_destination AND type = 'DROP_DOWN' AND field3 = ls_config_spe-field1 AND
                                                                                                                       field5 EQ 'BSR' AND default_ind = ' '.
          IF sy-subrc NE 0.
            SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'DESTINATION' AND type = 'DROP_DOWN' AND field5 EQ 'BSR' AND default_ind = ' '.
          ENDIF.
        ELSE.
          REFRESH lt_config.
          SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'DESTINATION' AND type = 'DROP_DOWN' AND field5 EQ 'BSR' AND default_ind = ' '.
        ENDIF.
        CHECK sy-subrc IS INITIAL.
        LOOP AT lt_config ASSIGNING <fs1> .
          APPEND INITIAL LINE TO lt_default_des ASSIGNING FIELD-SYMBOL(<fs_des>).
          <fs_des>-destination     = <fs1>-field1.
          <fs_des>-destination_txt = <fs1>-field2.
        ENDLOOP.
        copy_data_to_ref(
       EXPORTING
       is_data = lt_default_des
       CHANGING
       cr_data = er_entityset ).

      WHEN 'BSRReqStatus'.                " req type status
        REFRESH lt_config.
        SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'REQ_STATUS' AND type = 'DROP_DOWN' AND field5 EQ 'BSR' AND default_ind = ' '.
        CHECK sy-subrc IS INITIAL.
        LOOP AT lt_config ASSIGNING <fs1>.
          APPEND INITIAL LINE TO lt_default_rt_status ASSIGNING FIELD-SYMBOL(<fs_rts>).
          <fs_rts>-req_status = <fs1>-field1.
          <fs_rts>-req_status_txt = <fs1>-field2.
        ENDLOOP.
        copy_data_to_ref(
        EXPORTING
        is_data = lt_default_rt_status
        CHANGING
        cr_data = er_entityset ).

      WHEN  'BSRSpecInstr'.               " specific instructions
        REFRESH lt_config.
        SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'SPECIFIC_INSTRUCTION' AND type = 'DROP_DOWN' AND field5 EQ 'BSR'
                                                                                                           AND ( default_ind = ' ' OR default_ind = '1300' )."AND default_ind = ' '.
        CHECK sy-subrc IS INITIAL.
        lt_filters = io_tech_request_context->get_filter( )->get_filter_select_options( ).
        READ TABLE lt_filters INTO DATA(ls_filter) INDEX 1.
        IF sy-subrc IS INITIAL AND ls_filter-property = 'SPECIAL_INSTR'.
          SELECT * FROM zbucp_config INTO TABLE @DATA(lt_config_sp) WHERE name EQ 'SPECIAL_INSTRUCTION' AND type   = 'DROP_DOWN'
                                                                                                        AND field5 EQ 'BSR'" AND default_ind = ' '.
                                                                                                        AND ( default_ind = ' ' OR default_ind = '1300' ).
          READ TABLE ls_filter-select_options INTO DATA(ls_sel) INDEX 1.
          READ TABLE lt_config_sp INTO DATA(ls_config_sp) WITH KEY field1 = ls_sel-low.
          LOOP AT lt_config ASSIGNING <fs1> WHERE field4 = ls_config_sp-field2.
            APPEND INITIAL LINE TO lt_default_sp ASSIGNING FIELD-SYMBOL(<fs_sp>).
            <fs_sp>-specific_inst = <fs1>-field1.
            <fs_sp>-specific_inst_txt = <fs1>-field2.
            <fs_sp>-special_instr = ls_config_sp-field3.
          ENDLOOP.
          SELECT * FROM zbucp_config INTO TABLE @DATA(lt_rem_spefc_ins) WHERE name EQ 'SPECIAL_INSTRUCTION' AND type   = 'DROP_DOWN'
                                                                                                        and field1 eq @ls_sel-low
                                                                                                        AND field5 EQ 'BSR' .
          IF lt_rem_spefc_ins IS NOT INITIAL.
            LOOP at lt_rem_spefc_ins ASSIGNING FIELD-SYMBOL(<fs_rem_specf>).
              delete lt_default_sp WHERE specific_inst = <fs_rem_specf>-field2.
            ENDLOOP.
          ENDIF.
        ELSEIF ls_filter-property = 'SPECIFIC_INST'.
          READ TABLE ls_filter-select_options INTO ls_sel INDEX 1.
          SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'SPECIFIC_INSTRUCTION' AND type = 'DROP_DOWN'
                                                                                               AND field1 = ls_sel-low AND field5 EQ 'BSR'" AND default_ind = ' '.
                                                                                               AND ( default_ind = ' ' OR default_ind = '1300' ).
          LOOP AT lt_config ASSIGNING <fs1>.
            APPEND INITIAL LINE TO lt_default_sp ASSIGNING <fs_sp>.
            <fs_sp>-specific_inst = <fs1>-field1.
            <fs_sp>-specific_inst_txt = <fs1>-field2.
            <fs_sp>-special_instr = <fs1>-field3.
          ENDLOOP.
        ENDIF.

        copy_data_to_ref(
        EXPORTING
        is_data = lt_default_sp
        CHANGING
        cr_data = er_entityset ).

      WHEN  'BSRSplInstr' .                " special instructions
        REFRESH:lt_filters,lt_config.
        CLEAR ls_filter.
        lt_filters = io_tech_request_context->get_filter( )->get_filter_select_options( ).
        READ TABLE lt_filters INTO ls_filter INDEX 1.
        IF sy-subrc IS INITIAL.
          SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'SPECIAL_INSTRUCTION' AND type = 'DROP_DOWN' AND field1 = ls_sel-low
                                                                                              AND field5 EQ 'BSR' AND ( default_ind = ' ' OR default_ind = '1300' ).
        ELSE.
          SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'SPECIAL_INSTRUCTION' AND type = 'DROP_DOWN' AND field5 EQ 'BSR' AND
                                                                                                                ( default_ind = ' ' OR default_ind = '1300' ).
        ENDIF.
        LOOP AT lt_config ASSIGNING <fs1>.
          APPEND INITIAL LINE TO lt_default_si ASSIGNING FIELD-SYMBOL(<fs_si>).
          <fs_si>-special_instr     = <fs1>-field1.
          <fs_si>-special_instr_txt = <fs1>-field2.
          <fs_si>-special_instr_key = <fs1>-field3.
        ENDLOOP.
        copy_data_to_ref(
        EXPORTING
        is_data = lt_default_si
        CHANGING
        cr_data = er_entityset ).
      WHEN  'BSRWip' .                     "
        REFRESH lt_config.
        SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'WIP_LOCATION' AND type = 'DROP_DOWN' AND field5 EQ 'BSR' AND default_ind = ' '.

        CHECK sy-subrc IS INITIAL.
        LOOP AT lt_config ASSIGNING <fs1> .
          APPEND INITIAL LINE TO lt_default_wip ASSIGNING FIELD-SYMBOL(<fs_wip>).
          <fs_wip>-wip_location = <fs1>-field1.
          <fs_wip>-wip_location_txt = <fs1>-field2.
        ENDLOOP.
        copy_data_to_ref(
        EXPORTING
        is_data = lt_default_wip
        CHANGING
        cr_data = er_entityset ).
      WHEN 'BSRUat' .
        REFRESH lt_config.
        SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'UNITS_NOW_AVAIL_AT' AND type = 'DROP_DOWN' AND field5 EQ 'BSR' AND default_ind = ' '.
        CHECK sy-subrc IS INITIAL.
        LOOP AT lt_config  ASSIGNING <fs1>.
          APPEND INITIAL LINE TO lt_default_una ASSIGNING FIELD-SYMBOL(<fs_una>).
          <fs_una>-units_now_ava_at = <fs1>-field1.
          <fs_una>-units_now_ava_at_txt = <fs1>-field2.
        ENDLOOP.
        copy_data_to_ref(
        EXPORTING
        is_data = lt_default_una
        CHANGING
        cr_data = er_entityset ).
      WHEN 'BSRType'  .                   " bsr types
        REFRESH lt_config.
        SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'BSR_TYPE' AND type = 'DROP_DOWN' AND field5 EQ 'BSR' AND default_ind = ' '.
        CHECK sy-subrc IS INITIAL.
        LOOP AT lt_config ASSIGNING <fs1> .
          APPEND INITIAL LINE TO lt_default_bsrtype ASSIGNING FIELD-SYMBOL(<fs_bsrt>).
          <fs_bsrt>-bsr_type = <fs1>-field1.
          <fs_bsrt>-bsr_type_txt = <fs1>-field2.
        ENDLOOP.
        copy_data_to_ref(
        EXPORTING
        is_data = lt_default_bsrtype
        CHANGING
        cr_data = er_entityset ).
      WHEN 'BSRPlant'  .                   " Plant codes
        REFRESH lt_config.
        SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'PLANT' AND type = 'DROP_DOWN' AND field5 EQ 'BSR' AND default_ind = ' '.
        CHECK sy-subrc IS INITIAL.
        LOOP AT lt_config ASSIGNING <fs1> .
          APPEND INITIAL LINE TO lt_default_plant ASSIGNING FIELD-SYMBOL(<fs_plant>).
          <fs_plant>-werks = <fs1>-field1.
          <fs_plant>-name1 = <fs1>-field2.
        ENDLOOP.
        copy_data_to_ref(
        EXPORTING
        is_data = lt_default_plant
        CHANGING
        cr_data = er_entityset ).
      WHEN 'BSRRouting'.
        lt_filters = io_tech_request_context->get_filter( )->get_filter_select_options( ).
        IF lt_filters IS INITIAL.

          SELECT plnnr, plnal,werks,plnnr_alt FROM plko INTO TABLE @DATA(lt_plko) WHERE plnty     EQ 'S'
                                                                                    AND verwe     EQ 'BSR'.
        ELSE.
          IF lines( lt_filters ) > 1.
            LOOP AT it_filter_select_options INTO DATA(ls_filter_select_options).
              READ TABLE ls_filter_select_options-select_options INTO DATA(ls_spe) INDEX 1.
              IF ls_filter_select_options-property = 'Werks'.
                DATA(lv_werks_gg8) = ls_spe-low.
              ELSEIF ls_filter_select_options-property = 'Tecn'.
                DATA(lv_tecn) = ls_spe-low.
              ELSEIF ls_filter_select_options-property  = 'SpecialInstr'.
                DATA(lv_speins_gg8) = ls_spe-low.
              ELSEIF ls_filter_select_options-property = 'Mfrpn'.
                DATA(lv_mfrpn_gg8) = ls_spe-low.
                CLEAR lv_ip_matnr.
                CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                  EXPORTING
                    input  = lv_mfrpn_gg8
                  IMPORTING
                    output = lv_ip_matnr.
              ENDIF.
            ENDLOOP.

            DATA(ls_char) = read_mat_char_identifier( iv_matnr = lv_ip_matnr ).
            "start changes return to tci
            SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'ZBSR_ROUT'
                              AND type = 'DROP_DOWN' AND field1 EQ lv_speins_gg8 AND field3 = lv_werks_gg8.
            IF sy-subrc EQ 0.
              LOOP AT lt_config ASSIGNING <fs1>.
                APPEND INITIAL LINE TO lt_routing ASSIGNING FIELD-SYMBOL(<fs_routing1>).
                <fs_routing1>-plnnr     = <fs1>-field2  && ` ` && <fs1>-default_ind.
                <fs_routing1>-plnnr_alt = <fs1>-field4.
              ENDLOOP.
            ELSE.
              SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'ZBSR_PLANT_INST'
                                            AND type = 'DROP_DOWN' AND field1 EQ lv_speins_gg8 .
              IF lt_config IS NOT INITIAL.
                LOOP AT lt_config ASSIGNING <fs1>.
                  APPEND INITIAL LINE TO lt_routing ASSIGNING <fs_routing1>.
                  <fs_routing1>-plnnr     = <fs1>-field2  && ` ` && <fs1>-default_ind.
                  <fs_routing1>-plnnr_alt = <fs1>-field4.
                ENDLOOP.
              ELSE.
                DATA(lv_all) = abap_true.
              ENDIF.
            ENDIF.
            IF lv_all = abap_true.
              " end changes return to TCI
              SELECT plnnr plnal werks plnnr_alt FROM plko INTO TABLE lt_plko WHERE plnty     EQ 'S'
                                                                                      AND werks   =  lv_werks_gg8
                                                                                      AND verwe     EQ 'BSR'.
              IF lv_speins_gg8 = '1'.
                SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'SPECIAL_INSTRUCTION' AND type = 'DROP_DOWN' AND field1 = lv_speins_gg8
                                                                                               AND field5 EQ 'BSR' AND default_ind = '1300'.
                IF sy-subrc EQ 0.
                  REFRESH lt_config[].
                  SELECT * FROM zbucp_config INTO TABLE lt_config
                               WHERE name IN ( 'ZBSR_ROUT' )
                              AND type = 'DROP_DOWN'
                             AND field3 = lv_werks_gg8.
                ELSE.
                  SELECT * FROM zbucp_config INTO TABLE lt_config
                              WHERE name IN ( 'ZBSR_ROUT', 'ZBSR_TECN' )
                             AND type = 'DROP_DOWN'.

                ENDIF.

                LOOP AT lt_config ASSIGNING <fs1>.
                  READ TABLE lt_plko TRANSPORTING NO FIELDS WITH KEY plnnr = <fs1>-field2.
                  IF sy-subrc EQ 0.
                    DELETE lt_plko INDEX sy-tabix.
                  ENDIF.
                ENDLOOP.


              ENDIF.
            ENDIF.

          ELSE.
            "gg8
            READ TABLE lt_filters INTO ls_filter INDEX 1.
            IF sy-subrc IS INITIAL.
              CLEAR ls_sel.
              READ TABLE ls_filter-select_options INTO ls_sel INDEX 1.
              IF NOT ls_sel-low IS INITIAL.
                SELECT plnnr plnal werks plnnr_alt FROM plko INTO TABLE lt_plko WHERE plnty     EQ 'S'
                                                                                  AND werks     IN ls_filter-select_options
                                                                                  AND verwe     EQ 'BSR'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        " sjain36278
        IF lv_tecn IS NOT INITIAL OR ls_char-value_neutral = 'M'.
          REFRESH : lt_config[],lt_routing[].
          SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'ZBSR_TECN'
               AND type = 'DROP_DOWN' AND field3 = lv_werks_gg8.
          IF lt_config IS NOT INITIAL.
            LOOP AT lt_config ASSIGNING <fs1>.
              APPEND INITIAL LINE TO lt_routing ASSIGNING <fs_routing1>.
              <fs_routing1>-plnnr     = <fs1>-field2  && ` ` && <fs1>-default_ind.
              <fs_routing1>-plnnr_alt = <fs1>-field4.
            ENDLOOP.
          ENDIF.

        ELSE.
*          changes to remove tecn group
          REFRESH lt_config[].
          SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'ZBSR_TECN' "#EC CI_ALL_FIELDS_NEEDED
               AND type = 'DROP_DOWN' AND field3 IN ls_filter-select_options. "#EC CI_ALL_FIELDS_NEEDED
*          changes to remove tecn group
          LOOP AT lt_plko ASSIGNING FIELD-SYMBOL(<fs_plko>) .
            READ TABLE lt_config TRANSPORTING NO FIELDS WITH KEY field2 = <fs_plko>-plnnr .
            IF sy-subrc EQ 4.
              APPEND INITIAL LINE TO lt_routing ASSIGNING FIELD-SYMBOL(<fs_routing>).
              <fs_routing>-plnnr_alt = <fs_plko>-plnnr_alt.
              <fs_routing>-plnnr     = <fs_plko>-plnnr && ` ` && <fs_plko>-plnal.
              <fs_routing>-werks     = <fs_plko>-werks.
            ENDIF.
          ENDLOOP.
        ENDIF.
        "sjain36278

        copy_data_to_ref(
        EXPORTING
        is_data = lt_routing
        CHANGING
        cr_data = er_entityset ).
      WHEN 'BSRPl'  .                    " product line
        REFRESH lt_config.
        SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'PRODUCT_LINE' AND type = 'DROP_DOWN' AND field5 EQ 'BSR' AND default_ind = ' '. "#EC CI_ALL_FIELDS_NEEDED
        CHECK sy-subrc IS INITIAL.
        LOOP AT lt_config ASSIGNING <fs1>.
          APPEND INITIAL LINE TO lt_default_pl ASSIGNING FIELD-SYMBOL(<fs_pl>).
          <fs_pl>-product_line = <fs1>-field1.
          <fs_pl>-product_line_txt = <fs1>-field2.
        ENDLOOP.
        copy_data_to_ref(
        EXPORTING
        is_data = lt_default_pl
        CHANGING
        cr_data = er_entityset ).
      WHEN 'BSRStatusLog'  .
        REFRESH lt_config.
        lt_filters = io_tech_request_context->get_filter( )->get_filter_select_options( ).
        CHECK NOT lt_filters IS INITIAL.
        READ TABLE lt_filters INTO ls_filter INDEX 1.
        READ TABLE ls_filter-select_options INTO ls_sel INDEX 1.
        lv_reqid = ls_sel-low.
        SELECT * FROM zbsr_status_log INTO CORRESPONDING FIELDS OF TABLE lt_statuslog WHERE req_id EQ lv_reqid.
        SELECT * FROM zbucp_config INTO TABLE lt_config WHERE name EQ 'REQ_STATUS' AND type = 'DROP_DOWN' AND field5 EQ 'BSR' AND default_ind = ' '. "#EC CI_ALL_FIELDS_NEEDED
        IF NOT lt_statuslog IS INITIAL.
          SELECT bname, accnt FROM usr02 INTO TABLE @DATA(lt_usr02) FOR ALL ENTRIES IN @lt_statuslog WHERE bname = @lt_statuslog-zwho.
          IF sy-subrc IS INITIAL.
            LOOP AT lt_usr02 INTO DATA(ls_usr02).
              APPEND INITIAL LINE TO ra_emp ASSIGNING FIELD-SYMBOL(<fs_emp>).
              <fs_emp>-sign    = 'I'.
              <fs_emp>-option  = 'EQ'.
              <fs_emp>-low     = ls_usr02-accnt.
            ENDLOOP.
            SELECT employee_id,sapusername, displayname FROM zworkday_data INTO TABLE @DATA(lt_workday) WHERE employee_id IN @ra_emp.
          ENDIF.
        ENDIF.
        LOOP AT lt_statuslog ASSIGNING FIELD-SYMBOL(<fs_es>).
          IF <fs_es>-zwho IS ASSIGNED.
            READ TABLE lt_usr02 INTO ls_usr02 WITH KEY bname = <fs_es>-zwho.
            IF sy-subrc IS INITIAL.
              READ TABLE lt_workday INTO DATA(ls_workday) WITH KEY employee_id = ls_usr02-accnt.
              IF sy-subrc IS INITIAL.
                <fs_es>-name = ls_workday-displayname.
              ENDIF.
            ENDIF.
          ENDIF.
          IF <fs_es>-new_status IS ASSIGNED.
            READ TABLE lt_config INTO DATA(ls_config) WITH KEY field1 = <fs_es>-new_status.
            IF sy-subrc IS INITIAL.
              <fs_es>-new_status_txt = ls_config-field2.
            ENDIF.
          ENDIF.
          IF <fs_es>-old_status IS ASSIGNED.
            READ TABLE lt_config INTO ls_config WITH KEY field1 = <fs_es>-old_status.
            IF sy-subrc IS INITIAL.
              <fs_es>-old_status_txt = ls_config-field2.
            ENDIF.
          ENDIF.
          CLEAR: ls_workday, ls_config.
        ENDLOOP.
        copy_data_to_ref(
        EXPORTING
          is_data = lt_statuslog
          CHANGING
          cr_data = er_entityset ).
      WHEN 'BSRAuth'.
        DATA lt_auth TYPE  zcl_zbsr_form_mpc_ext=>tt_bsrauth.
        DATA ls_auth TYPE  zcl_zbsr_form_mpc_ext=>ts_bsrauth.
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
          ELSE.
            lv_role = 'REQ'.
          ENDIF.
        ENDIF.

        CHECK NOT lv_role IS INITIAL.
        AUTHORITY-CHECK OBJECT 'ZBSR_FORM' ID 'BSR_AUTH' FIELD lv_role.
        IF sy-subrc IS INITIAL.
          ls_auth-success = 'Y'.
          IF lv_status LE 2.
*            CALL FUNCTION 'ENQUEUE_EZBSR_REQ_ID'
*              EXPORTING
*                mode_zbsr_header = 'E'
*                req_id           = lv_reqid
*              EXCEPTIONS
*                foreign_lock     = 1
*                system_failure   = 2
*                OTHERS           = 3.
*            IF sy-subrc <> 0.
            IF lo_exception IS BOUND.
              DATA lv_msg TYPE bapi_msg .
              CALL FUNCTION 'FORMAT_MESSAGE'
                EXPORTING
                  id        = sy-msgid
                  lang      = 'EN'
                  no        = sy-msgno
                  v1        = sy-msgv1
                  v2        = sy-msgv2
                  v3        = sy-msgv3
                  v4        = sy-msgv4
                IMPORTING
                  msg       = lv_msg
                EXCEPTIONS
                  not_found = 1
                  OTHERS    = 2.
              IF sy-subrc <> 0.
* Implement suitable error handling here
              ENDIF.
              lo_exception->get_msg_container( )->add_message(
                       EXPORTING
                         iv_msg_type     = sy-msgty
                         iv_msg_id      = sy-msgid
                         iv_msg_number  = sy-msgno
                         iv_msg_text    = lv_msg ).
*          IV_MSG_V1
              RAISE EXCEPTION lo_exception.
            ENDIF.
          ENDIF.
*          ENDIF.
        ELSE.
          ls_auth-success = 'N'.
        ENDIF.
        ls_auth-auth    = lv_role.
        ls_auth-req_id  = lv_reqid.
        APPEND ls_auth TO lt_auth.
        CLEAR ls_auth.
        copy_data_to_ref(
        EXPORTING
          is_data = lt_auth
           CHANGING
          cr_data = er_entityset ).
      WHEN 'BSRMat'.
        DATA lt_mat TYPE  zcl_zbsr_form_mpc_ext=>tt_bsrmat.
        copy_data_to_ref(
      EXPORTING
        is_data = lt_mat
         CHANGING
        cr_data = er_entityset ).

      WHEN 'BSRBatchSH'.
        lt_filters = io_tech_request_context->get_filter( )->get_filter_select_options( ).
        READ TABLE lt_filters INTO ls_filter INDEX 1.
        LOOP AT lt_filters INTO ls_filter.
          CASE ls_filter-property.
            WHEN 'LICHA'.
              LOOP AT ls_filter-select_options ASSIGNING FIELD-SYMBOL(<fs_sel_opt>)  WHERE low IS NOT INITIAL  .
                APPEND INITIAL LINE TO ra_licha ASSIGNING FIELD-SYMBOL(<fs_licha>).
                MOVE-CORRESPONDING <fs_sel_opt> TO <fs_licha>.
              ENDLOOP.
            WHEN 'CHARG'.
              LOOP AT ls_filter-select_options ASSIGNING <fs_sel_opt>  WHERE low IS NOT INITIAL  .
                APPEND INITIAL LINE TO ra_charg ASSIGNING FIELD-SYMBOL(<fs_charg>).
                MOVE-CORRESPONDING <fs_sel_opt> TO <fs_charg>.
              ENDLOOP.
            WHEN 'WERKS'.
              LOOP AT ls_filter-select_options ASSIGNING <fs_sel_opt>  WHERE low IS NOT INITIAL .
                APPEND INITIAL LINE TO ra_werks ASSIGNING FIELD-SYMBOL(<fs_werks>).
                MOVE-CORRESPONDING <fs_sel_opt> TO <fs_werks>.
              ENDLOOP.
            WHEN 'MFRPN'.
              DATA lv_matnr TYPE matnr.
              LOOP AT ls_filter-select_options ASSIGNING <fs_sel_opt> WHERE low IS NOT INITIAL .
                APPEND INITIAL LINE TO ra_matnr ASSIGNING FIELD-SYMBOL(<fs_matnr>).
                MOVE-CORRESPONDING <fs_sel_opt> TO <fs_matnr>.
              ENDLOOP.
            WHEN 'WIP_LOCATION'.
              LOOP AT ls_filter-select_options ASSIGNING <fs_sel_opt> WHERE low IS NOT INITIAL  .
                APPEND INITIAL LINE TO ra_lgort ASSIGNING FIELD-SYMBOL(<fs_lgort>).
                MOVE-CORRESPONDING <fs_sel_opt> TO <fs_lgort>.
              ENDLOOP.
          ENDCASE.
        ENDLOOP.
        IF NOT ra_matnr IS INITIAL.
*118276
          SELECT matnr, werks, lgort, charg, clabs , cinsm, ceinm FROM mchb INTO TABLE @DATA(lt_mchb) WHERE matnr IN @ra_matnr "#EC CI_NO_TRANSFORM
                                                                                         AND werks IN @ra_werks
                                                                                         AND lgort IN @ra_lgort
                                                                                         AND charg IN @ra_charg
                                                                                         AND ( clabs GT 0 OR cinsm GT 0 OR ceinm GT 0 ).
*118276

*          SELECT matnr, werks, lgort, charg, clabs FROM mchb INTO TABLE @DATA(lt_mchb) WHERE matnr IN @ra_matnr
*                                                                               AND werks IN @ra_werks
*                                                                               AND lgort IN @ra_lgort
*                                                                               AND charg IN @ra_charg
*                                                                               AND clabs GT 0.


          IF sy-subrc IS INITIAL.
            SELECT matnr, charg, licha FROM mch1 INTO TABLE @DATA(lt_mch1) FOR ALL ENTRIES IN @lt_mchb WHERE matnr EQ @lt_mchb-matnr "#EC CI_NO_TRANSFORM
                                                                                                  AND charg EQ @lt_mchb-charg.
            LOOP AT lt_mchb INTO DATA(ls_mchb).
              READ TABLE lt_mch1 INTO DATA(ls_mch1) WITH KEY matnr = ls_mchb-matnr charg = ls_mchb-charg.
              APPEND INITIAL LINE TO lt_batch ASSIGNING FIELD-SYMBOL(<fs_batch>).
              <fs_batch>-licha = ls_mch1-licha.
              <fs_batch>-clabs = ls_mchb-clabs.
              <fs_batch>-cinsm  = ls_mchb-cinsm.            " 118276
              <fs_batch>-ceinm  = ls_mchb-ceinm.            " 118276
              <fs_batch>-charg =  ls_mchb-charg.
            ENDLOOP.
          ENDIF.
        ELSE.
          IF lo_exception IS BOUND.

            lo_exception->get_msg_container( )->add_message(
           EXPORTING
             iv_msg_type     = 'E'
             iv_msg_id      = '00'
             iv_msg_number  = '123'
             iv_msg_text    = 'Material Input is mandatory to proceed with search'  ).
*          IV_MSG_V1
            RAISE EXCEPTION lo_exception.
            RETURN.
          ENDIF.
        ENDIF.
        copy_data_to_ref(
EXPORTING
  is_data = lt_batch
   CHANGING
  cr_data = er_entityset ).
    ENDCASE.
    "gg8 scenario
*              READ TABLE lt_filters INTO ls_filter INDEX 1.
*              IF sy-subrc IS INITIAL.
*              CLEAR ls_sel.
*              READ TABLE ls_filter-select_options INTO ls_sel INDEX 1.
*                IF NOT ls_sel-low IS INITIAL.
*                  SELECT plnnr plnal werks plnnr_alt FROM plko INTO TABLE lt_plko WHERE plnty     EQ 'N'
*                                                                                    AND werks     IN ls_filter-select_options
*                                                                                    AND verwe     EQ 'BSR'.
*              SELECT matnr,werks,plnty,plnnr INTO TABLE @DATA(lt_mapl)
*              FROM mapl
*              WHERE matnr EQ @lv_ip_matnr
*              AND werks EQ @lv_werks_gg8
*              AND plnty EQ 'N'.
*              IF sy-subrc EQ 0.
*                DELETE ADJACENT DUPLICATES FROM lt_mapl COMPARING plnty plnnr.
*                SELECT plnnr plnal werks plnnr_alt FROM plko
*                    INTO TABLE lt_plko
*                  FOR ALL ENTRIES IN lt_mapl
*                  WHERE plnty = lt_mapl-plnty
*                  AND plnnr = lt_mapl-plnnr
*                  AND werks = lt_mapl-werks.
**                                                                              AND verwe     EQ 'BSR'.
*                IF sy-subrc EQ 0.
*
*                ENDIF.
*              ENDIF.
  ENDMETHOD.


  METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY.
    DATA: ls_bsr_head_item    TYPE zcl_zbsr_form_mpc_ext=>ts_head_item,
          lt_bsr_head_item    TYPE TABLE OF zcl_zbsr_form_mpc_ext=>ts_head_item,
          ls_expanded_clause1 LIKE LINE OF et_expanded_tech_clauses,
          lv_req              TYPE zbsr_header-req_id,
          lv_name             TYPE thead-tdname,
          ls_char             TYPE bapi1003_alloc_values_char.

    CASE iv_entity_name.
      WHEN 'BSRHeader'.
        READ TABLE it_key_tab INTO DATA(ls_key) INDEX 1.
        lv_req = ls_key-value.
        CHECK NOT lv_req IS INITIAL.
        SELECT SINGLE * FROM zbsr_header INTO @DATA(ls_header) WHERE req_id = @lv_req.
        CHECK sy-subrc IS INITIAL.
        SELECT * FROM zbsr_item INTO TABLE @DATA(lt_item) WHERE req_id = @lv_req.  "#EC CI_ALL_FIELDS_NEEDED
        SELECT * FROM zbsr_notifcation INTO TABLE @DATA(lt_notif) WHERE req_id = @lv_req. "#EC CI_ALL_FIELDS_NEEDED
        MOVE-CORRESPONDING ls_header TO ls_bsr_head_item.
        SELECT SINGLE accnt FROM usr02 INTO @DATA(lv_accnt) WHERE bname EQ @ls_header-created_by.
        IF sy-subrc IS INITIAL.
          SELECT SINGLE sapusername , displayname, managerid FROM zworkday_data INTO @DATA(ls_workday) WHERE employee_id EQ @lv_accnt.
          IF sy-subrc IS INITIAL.
            ls_bsr_head_item-createdonuser = ls_workday-displayname.
            SELECT SINGLE sapusername displayname managerid FROM zworkday_data INTO ls_workday WHERE employee_id EQ ls_workday-managerid.
            IF sy-subrc IS INITIAL.
              ls_bsr_head_item-manager = ls_workday-displayname.
            ENDIF.
          ENDIF.
        ENDIF.
        IF NOT ls_header-plnnr IS INITIAL.
          ls_bsr_head_item-plnnr_alt = ls_header-plnnr && ` ` && ls_header-plnal.
        ENDIF.
        IF NOT ls_header-changed_by IS INITIAL.
          CLEAR ls_workday.
          SELECT SINGLE accnt FROM usr02 INTO lv_accnt WHERE bname EQ ls_header-changed_by.
          SELECT SINGLE sapusername displayname managerid FROM zworkday_data INTO ls_workday WHERE employee_id EQ lv_accnt.
          ls_bsr_head_item-changedonuser = ls_workday-displayname.
        ENDIF.

        IF NOT ls_header-output_material IS INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
            EXPORTING
              input  = ls_header-output_material
            IMPORTING
              output = ls_bsr_head_item-output_material.
        ENDIF.

        IF NOT ls_header-sap_material IS INITIAL.
          SELECT SINGLE matnr, mtart, meins FROM mara INTO @DATA(ls_mara_tmp)  WHERE matnr EQ @ls_header-sap_material.
          ls_bsr_head_item-uom = ls_mara_tmp-meins.
          ls_char = read_mat_char_identifier( iv_matnr = ls_header-sap_material ).
          ls_bsr_head_item-product_line = ls_char-value_neutral.
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
            EXPORTING
              input  = ls_header-sap_material
            IMPORTING
              output = ls_bsr_head_item-sap_material.
        ENDIF.

        IF lv_req IS INITIAL .
          lv_req = ls_bsr_head_item-req_id.
        ENDIF.
        CHECK NOT lv_req IS INITIAL.
        lv_name = sy-mandt && lv_req.
        ls_bsr_head_item-addtext =  read_text(
             EXPORTING
               id        = zcl_zbsr_form_mpc_ext=>gc_tdid_at    " Text ID
               name      = lv_name    " Name
               object    = zcl_zbsr_form_mpc_ext=>gc_object ).   " Texts: application object

        ls_bsr_head_item-destination =  read_text(
             EXPORTING
               id        = zcl_zbsr_form_mpc_ext=>gc_tdid_des    " Text ID
               name      = lv_name    " Name
               object    = zcl_zbsr_form_mpc_ext=>gc_object ).   " Texts: application object
        ls_bsr_head_item-location =  read_text(
       EXPORTING
         id        = zcl_zbsr_form_mpc_ext=>gc_tdid_loc    " Text ID
         name      = lv_name    " Name
         object    = zcl_zbsr_form_mpc_ext=>gc_object ).   " Texts: application object

        ls_bsr_head_item-reasforincomplete =  read_text(
     EXPORTING
       id        = zcl_zbsr_form_mpc_ext=>gc_tdid_inc    " Text ID
       name      = lv_name    " Name
       object    = zcl_zbsr_form_mpc_ext=>gc_object  ).  " Texts: application object

        ls_bsr_head_item-purpose =  read_text(
       EXPORTING
         id        = zcl_zbsr_form_mpc_ext=>gc_tdid_res    " Text ID
         name      = lv_name    " Name
         object    = zcl_zbsr_form_mpc_ext=>gc_object ).   " Texts: application object

        copy_data_to_ref(
            EXPORTING
              is_data = ls_bsr_head_item
            CHANGING
              cr_data = er_entity ).
    ENDCASE.

  ENDMETHOD.


  METHOD /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET.
    DATA: ls_bsr_head_item    TYPE zcl_zbsr_form_mpc_ext=>ts_head_item,
          lt_bsr_head_item    TYPE TABLE OF zcl_zbsr_form_mpc_ext=>ts_head_item,
          lt_head             TYPE TABLE OF zcl_zbsr_form_mpc_ext=>ts_bsrheader,
          lt_item             TYPE TABLE OF zcl_zbsr_form_mpc_ext=>ts_bsritem,
          lt_notif            TYPE TABLE OF zcl_zbsr_form_mpc_ext=>ts_bsrnotifications,
          ls_expanded_clause1 LIKE LINE OF et_expanded_tech_clauses,
          lv_req              TYPE zbsr_header-req_id.

    CASE iv_entity_set_name .
      WHEN 'BSRHeaderSet'.

        READ TABLE it_key_tab INTO DATA(ls_data) INDEX 1.
        lv_req = ls_data-value.
        CHECK NOT lv_req IS INITIAL.
        SELECT SINGLE * FROM zbsr_header INTO @DATA(ls_header) WHERE req_id = @lv_req. "#CI_ALL_FIELDS_NEEDED
        CHECK sy-subrc IS INITIAL.
        MOVE-CORRESPONDING ls_header TO ls_bsr_head_item.
        SELECT SINGLE bname, accnt FROM usr02 INTO @DATA(ls_usr02) WHERE bname EQ @ls_header-created_by.
        IF sy-subrc IS INITIAL.
          SELECT SINGLE sapusername, displayname, managerid FROM zworkday_data INTO @DATA(ls_workday) WHERE employee_id EQ @ls_usr02-accnt.
          IF sy-subrc IS INITIAL.
            ls_bsr_head_item-createdonuser = ls_workday-displayname.
            SELECT SINGLE sapusername displayname managerid FROM zworkday_data INTO ls_workday WHERE employee_id EQ ls_workday-managerid.
            ls_bsr_head_item-manager = ls_workday-displayname.
          ENDIF.
        ENDIF.
        SELECT SINGLE bname accnt FROM usr02 INTO ls_usr02 WHERE bname EQ ls_header-changed_by.
        IF sy-subrc IS INITIAL.
          SELECT SINGLE sapusername displayname managerid FROM zworkday_data INTO ls_workday WHERE employee_id EQ ls_usr02-accnt.
          ls_bsr_head_item-changedonuser = ls_workday-displayname.
        ENDIF.

        copy_data_to_ref(
          EXPORTING
            is_data = lt_bsr_head_item
          CHANGING
            cr_data = er_entityset ).

      WHEN 'BSRItemSet' .
        READ TABLE it_key_tab INTO ls_data INDEX 1.
        lv_req = ls_data-value.
        CHECK NOT lv_req IS INITIAL.
        SELECT * FROM zbsr_item INTO TABLE @DATA(lt_item1) WHERE req_id = @lv_req.

        LOOP AT lt_item1 INTO DATA(ls_item).
          APPEND INITIAL LINE TO lt_item ASSIGNING FIELD-SYMBOL(<fs>).
          MOVE-CORRESPONDING ls_item TO <fs>.
        ENDLOOP.
        copy_data_to_ref(
          EXPORTING
            is_data = lt_item
          CHANGING
            cr_data = er_entityset ).
        APPEND ls_bsr_head_item TO lt_bsr_head_item.
        ls_expanded_clause1  = 'HEADITNAV'.
        APPEND ls_expanded_clause1 TO et_expanded_tech_clauses.
        ls_expanded_clause1  = 'ITNOTIFNAV'.
        APPEND ls_expanded_clause1 TO et_expanded_tech_clauses.
      WHEN 'BSRNotificationsSet'.
        READ TABLE it_key_tab INTO ls_data INDEX 1.
        lv_req = ls_data-value.
        CHECK NOT lv_req IS INITIAL.
        SELECT * FROM zbsr_notifcation INTO TABLE @DATA(lt_notif1) WHERE req_id = @lv_req. "#EC CI_ALL_FIELDS_NEEDED

        LOOP AT lt_notif1 INTO DATA(ls_notif).
          APPEND INITIAL LINE TO lt_notif ASSIGNING FIELD-SYMBOL(<fs_notif>).
          MOVE-CORRESPONDING ls_notif TO <fs_notif>.
        ENDLOOP.
        ls_expanded_clause1  = 'HEADITNAV'.
        APPEND ls_expanded_clause1 TO et_expanded_tech_clauses.
        ls_expanded_clause1  = 'ITNOTIFNAV'.
        APPEND ls_expanded_clause1 TO et_expanded_tech_clauses.
        copy_data_to_ref(
 EXPORTING
   is_data = lt_notif
 CHANGING
   cr_data = er_entityset ).

    ENDCASE.


  ENDMETHOD.


  method BSRAUTHSET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->BSRAUTHSET_GET_ENTITY
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


  method BSRAUTHSET_GET_ENTITYSET.
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
  endmethod.


  method BSRCONFIGSET_GET_ENTITYSET.

  SELECT * from ZBUCP_CONFIG into  TABLE ET_ENTITYSET WHERE NAME EQ 'SPECIAL_INSTRUCTION' OR NAME EQ 'SPECIFIC_INSTRUCTION'.

  endmethod.


  method BSRDEFAULTSSET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->BSRDEFAULTSSET_GET_ENTITY
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
    CASE iv_entity_set_name .
      WHEN 'BSRDefaultsSet'.
        DATA lt_default TYPE TABLE OF zbsr_s_defaults.
        APPEND INITIAL LINE TO lt_default ASSIGNING FIELD-SYMBOL(<fs1>).
        <fs1>-department = '01'.
        <fs1>-department_txt = 'ER'.
        <fs1>-req_type  = '01'.
        <fs1>-cost_center  = '7000'.
        <fs1>-product_line  = 'ATF'.
        <fs1>-special_instr  = 're'.
        <fs1>-wip_location  = '1200'.
        <fs1>-sap_material  = 'MFRPN1'.
        <fs1>-specific_inst  = 'tell'.
        <fs1>-completion_status  = '01'.
        <fs1>-units_now_ava_at  = '2300'.


        APPEND INITIAL LINE TO lt_default ASSIGNING <fs1>.
        <fs1>-department = '02'.
        <fs1>-department_txt = 'BR'.
        <fs1>-req_type  = '03'.
        <fs1>-cost_center  = '7002'.
        <fs1>-product_line  = 'ATE'.
        <fs1>-special_instr  = 'rw'.
        <fs1>-wip_location  = '1201'.
        <fs1>-sap_material  = 'MFRPN2'.
        <fs1>-specific_inst  = 'tel2'.
        <fs1>-completion_status  = '02'.
        <fs1>-units_now_ava_at  = '2320'.

        et_entityset = lt_default.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  method BSRDEPTSET_GET_ENTITYSET.
Select * FROM ZBUCP_CONFIG INto TABLE @DATA(lt_config) WHERE name EQ 'DEPARTMENT' AND type = 'DROP_DOWN' AND field5 EQ 'BSR'. "#EC CI_ALL_FIELDS_NEEDED
  CHECK sy-subrc IS INITIAL.
  LOOP AT lt_config ASSIGNING FIELD-SYMBOL(<fs1>).
  APPEND INITIAL LINE TO ET_ENTITYSET ASSIGNING FIELD-SYMBOL(<fs>).
  <fs>-department = <fs1>-field2.
  <fs>-department_txt = <fs1>-field3.
  ENDLOOP.
  endmethod.


  METHOD BSRHEADERSET_CREATE_ENTITY.
    DATA: ls_head_entity TYPE zcl_zbsr_form_mpc_ext=>ts_head_item,
          ls_header      TYPE zbsr_header,
          lv_reqid       TYPE zbsr_header-req_id,
          lv_reqid1      TYPE zbsr_header-req_id,
          lo_exception   TYPE REF TO /iwbep/cx_mgw_tech_exception,
          lv_name        TYPE thead-tdname,
          lv_matnr       TYPE matnr,
          lt_bsr_log     TYPE TABLE OF zbsr_status_log,
          ls_bsr_log     TYPE zbsr_status_log,
          ls_char        TYPE bapi1003_alloc_values_char.


    CLEAR: lv_reqid,ls_head_entity,ls_header,lv_name.
    CREATE OBJECT lo_exception.
    io_data_provider->read_entry_data(
   IMPORTING
   es_data = ls_head_entity ).
    IF ls_head_entity IS INITIAL AND lo_exception IS BOUND.
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

    IF NOT ls_head_entity-specific_inst IS INITIAL.
      SELECT SINGLE * FROM zbucp_config INTO @DATA(ls_config) WHERE name EQ 'SPECIFIC_INSTRUCTION' AND type EQ 'DROP_DOWN' AND field1 EQ @ls_head_entity-specific_inst. "#EC CI_ALL_FIELDS_NEEDED
      IF sy-subrc IS INITIAL AND ls_config-field3 EQ 1 AND ls_head_entity-plnnr_alt IS INITIAL.
        lo_exception->get_msg_container( )->add_message(
                   EXPORTING
                     iv_msg_type     = 'E'
                     iv_msg_id      = '00'
                     iv_msg_number  = '123'
                     iv_msg_text    = 'Route Name is mandatory' ).
        RAISE EXCEPTION lo_exception.
        RETURN.
      ENDIF.
    ENDIF.
    IF NOT ls_head_entity-special_instr IS INITIAL.
      CLEAR ls_config.
      SELECT SINGLE * FROM zbucp_config INTO ls_config WHERE name EQ 'SPECIAL_INSTRUCTION' AND type EQ 'DROP_DOWN' AND field1 EQ ls_head_entity-special_instr. "#EC CI_ALL_FIELDS_NEEDED
      IF sy-subrc IS INITIAL AND ls_config-field3 EQ 1 AND ls_head_entity-destination IS INITIAL.
        lo_exception->get_msg_container( )->add_message(
                   EXPORTING
                     iv_msg_type     = 'E'
                     iv_msg_id      = '00'
                     iv_msg_number  = '123'
                     iv_msg_text    = 'Destination is mandatory' ).
        RAISE EXCEPTION lo_exception.
        RETURN.
      ENDIF.
    ENDIF.
    IF ls_head_entity-req_status_sub EQ '1' AND ls_head_entity-headitnav IS INITIAL.
      lo_exception->get_msg_container( )->add_message(
         EXPORTING
           iv_msg_type     = 'E'
           iv_msg_id      = '00'
           iv_msg_number  = '123'
           iv_msg_text    = 'Batch details are mandatory to Submit the form' ).
*          IV_MSG_V1
      RAISE EXCEPTION lo_exception.
      RETURN.
    ENDIF.
    WRITE ls_head_entity-req_id TO lv_reqid NO-ZERO.
    CONDENSE lv_reqid.
    IF ls_head_entity-req_id NE '*'.
      SELECT SINGLE * FROM zbsr_header INTO @DATA(ls_header_db) WHERE req_id EQ @lv_reqid. "#EC CI_ALL_FIELDS_NEEDED
    ENDIF.
    MOVE-CORRESPONDING ls_head_entity TO ls_header.
    IF NOT ls_head_entity-output_material IS INITIAL.
      TRANSLATE ls_head_entity-output_material TO UPPER CASE.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = ls_head_entity-output_material
        IMPORTING
          output = lv_matnr.
      ls_header-output_material = lv_matnr.
      SELECT SINGLE matnr FROM mara INTO @DATA(lv_matnr_tmp) WHERE matnr EQ @lv_matnr.
      IF NOT sy-subrc IS INITIAL.
        lo_exception->get_msg_container( )->add_message(
             EXPORTING
               iv_msg_type     = 'E'
               iv_msg_id      = '00'
               iv_msg_number  = '123'
               iv_msg_text    = 'Output Material' && ` ` && ls_head_entity-output_material && ` ` && 'does not exist' ).
        RAISE EXCEPTION lo_exception.
        RETURN.
      ENDIF.
    ENDIF.

    IF NOT ls_head_entity-sap_material IS INITIAL.
      TRANSLATE ls_head_entity-sap_material TO UPPER CASE.
      CLEAR lv_matnr.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = ls_head_entity-sap_material
        IMPORTING
          output = lv_matnr.
      ls_header-sap_material = lv_matnr.
      DATA(lv_matnr_uom)     = ls_header-sap_material.
      SELECT SINGLE matnr FROM mara INTO lv_matnr_tmp WHERE matnr EQ lv_matnr.
      IF NOT sy-subrc IS INITIAL.
        lo_exception->get_msg_container( )->add_message(
             EXPORTING
               iv_msg_type     = 'E'
               iv_msg_id      = '00'
               iv_msg_number  = '123'
               iv_msg_text    = 'Input Material' && ` ` && ls_head_entity-sap_material && ` ` && 'does not exist' ).
        RAISE EXCEPTION lo_exception.
        RETURN.

      ENDIF.
      ls_char = read_mat_char_identifier( iv_matnr = lv_matnr  ).
      ls_header-product_line = ls_char-value_neutral.
      ls_head_entity-product_line = ls_header-product_line.
      IF ls_head_entity-uom IS INITIAL.
        SELECT SINGLE meins FROM mara INTO @DATA(lv_meins) WHERE matnr EQ @lv_matnr_uom.
        IF sy-subrc IS INITIAL.
          ls_header-uom = lv_meins.
          ls_head_entity-uom = lv_meins.
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT ls_head_entity-od_no IS INITIAL.
      SELECT SINGLE vbeln FROM lips INTO @DATA(lv_vbeln) WHERE vbeln EQ @ls_head_entity-od_no AND matnr EQ @ls_header-sap_material .
      IF NOT sy-subrc IS INITIAL.
        lo_exception->get_msg_container( )->add_message(
        EXPORTING
          iv_msg_type     = 'E'
          iv_msg_id      = '00'
          iv_msg_number  = '123'
          iv_msg_text    = 'OD Number' && ` ` && ls_head_entity-od_no && ` ` && 'doesnt exist in the system'  ).
        RAISE EXCEPTION lo_exception.
        RETURN.
      ENDIF.
    ENDIF.

    IF ls_head_entity-req_id EQ '*'.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZBSR_NR'
*         QUANTITY                = '1'
*         SUBOBJECT               = ' '
*         TOYEAR                  = '0000'
          ignore_buffer           = 'X'
        IMPORTING
          number                  = lv_reqid1
*         QUANTITY                =
*         RETURNCODE              =
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0 AND lv_reqid IS INITIAL.
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
      ls_head_entity-req_id = lv_reqid.
      ls_header-req_id      = lv_reqid.
      DATA(lv_create) = abap_true.
    ENDIF.

    IF NOT lv_create IS INITIAL.
      ls_head_entity-created_by    = sy-uname.
      ls_head_entity-created_on    = sy-datum.
      ls_header-created_on         = sy-datum.
      ls_header-created_by         = sy-uname.
      ls_header-changed_by         = sy-uname.
      ls_header-changed_on         = sy-datum.
      ls_head_entity-changed_on    = sy-datum.
      ls_head_entity-changed_by    = sy-uname.
    ELSE.
      ls_head_entity-changed_by    = sy-uname.
      ls_head_entity-changed_on    = sy-datum.
      ls_header-changed_on         = sy-datum.
      ls_header-changed_by         = sy-uname.
      ls_head_entity-created_by    = ls_header_db-created_by.
      ls_head_entity-created_on    = ls_header_db-created_on.
      ls_header-created_on         = ls_header_db-created_on.
      ls_header-created_by         = ls_header_db-created_by.
    ENDIF.
    IF NOT ls_head_entity-created_by IS INITIAL.
      SELECT SINGLE accnt FROM usr02 INTO @DATA(lv_accnt) WHERE bname EQ @sy-uname.
      IF sy-subrc IS INITIAL.
        SELECT SINGLE sapusername, displayname, managerid FROM zworkday_data INTO @DATA(ls_workday) WHERE employee_id EQ @lv_accnt.
        IF sy-subrc IS INITIAL.
          ls_head_entity-createdonuser = ls_workday-displayname.
          SELECT SINGLE sapusername displayname managerid FROM zworkday_data INTO ls_workday WHERE employee_id EQ ls_workday-managerid.
          IF sy-subrc IS INITIAL.
            ls_head_entity-manager = ls_workday-displayname.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF NOT ls_head_entity-changed_by IS INITIAL.
      SELECT SINGLE accnt FROM usr02 INTO lv_accnt WHERE bname EQ sy-uname.
      IF sy-subrc IS INITIAL.
        SELECT SINGLE sapusername displayname managerid FROM zworkday_data INTO ls_workday WHERE employee_id EQ lv_accnt.
        IF sy-subrc IS INITIAL.
          ls_head_entity-changedonuser = ls_workday-displayname.
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT ls_header IS INITIAL.
      MODIFY zbsr_header FROM ls_header.
      MOVE-CORRESPONDING ls_header TO er_entity.
    ENDIF.
    IF NOT lv_reqid IS INITIAL .
      ls_bsr_log-req_id     = lv_reqid.
      ls_bsr_log-new_status = 0.
      GET TIME STAMP FIELD ls_bsr_log-zwhen.
      ls_bsr_log-zwho       = sy-uname.
      MODIFY zbsr_status_log FROM ls_bsr_log.
    ENDIF.

    lv_name = sy-mandt && lv_reqid.
    IF NOT ls_head_entity-addtext IS INITIAL.
      save_text(
        EXPORTING
          id        = zcl_zbsr_form_mpc_ext=>gc_tdid_at    " Text ID
          name      = lv_name    " Name
          object    = zcl_zbsr_form_mpc_ext=>gc_object    " Texts: application object
          text      = ls_head_entity-addtext ).
    ENDIF.
    IF NOT ls_head_entity-destination IS INITIAL.
      save_text(
         EXPORTING
           id        = zcl_zbsr_form_mpc_ext=>gc_tdid_des    " Text ID
           name      = lv_name    " Name
           object    = zcl_zbsr_form_mpc_ext=>gc_object    " Texts: application object
           text      = ls_head_entity-destination ).
    ENDIF.
    IF NOT ls_head_entity-location IS INITIAL.
      save_text(
   EXPORTING
     id        = zcl_zbsr_form_mpc_ext=>gc_tdid_loc    " Text ID
     name      = lv_name    " Name
     object    = zcl_zbsr_form_mpc_ext=>gc_object    " Texts: application object
     text      = ls_head_entity-location ).
    ENDIF.
    IF NOT ls_head_entity-reasforincomplete IS INITIAL.
      save_text(
   EXPORTING
     id        = zcl_zbsr_form_mpc_ext=>gc_tdid_inc    " Text ID
     name      = lv_name    " Name
     object    = zcl_zbsr_form_mpc_ext=>gc_object    " Texts: application object
     text      = ls_head_entity-reasforincomplete ).
    ENDIF.
    IF NOT ls_head_entity-purpose IS INITIAL.
      save_text(
     EXPORTING
       id        = zcl_zbsr_form_mpc_ext=>gc_tdid_res    " Text ID
       name      = lv_name    " Name
       object    = zcl_zbsr_form_mpc_ext=>gc_object    " Texts: application object
       text      = ls_head_entity-purpose ).
    ENDIF.
  ENDMETHOD.


  METHOD BSRHEADERSET_GET_ENTITY.
    READ TABLE it_key_tab INTO DATA(ls_key) INDEX 1.
    SELECT SINGLE * FROM zbsr_header INTO CORRESPONDING FIELDS OF er_entity WHERE  req_id = ls_key-value.
  ENDMETHOD.


  METHOD BSRHEADERSET_GET_ENTITYSET.
    READ TABLE it_key_tab INTO DATA(ls_key) INDEX 1.
    IF sy-subrc IS INITIAL.
      SELECT * FROM zbsr_header INTO TABLE @DATA(lt_header) WHERE req_id = @ls_key-value.
      et_entityset = lt_header.
    ENDIF.


  ENDMETHOD.


  method BSRITEMSET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->BSRITEMSET_GET_ENTITY
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
     DATA ls_item TYPE zcl_zbsr_form_mpc_ext=>ts_bsritem.

    er_entity = ls_item .

  endmethod.


  method BSRITEMSET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->BSRITEMSET_GET_ENTITYSET
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

    DATA lt_item TYPE STANDARD TABLE OF zcl_zbsr_form_mpc_ext=>ts_bsritem.

    et_entityset = lt_item .


  endmethod.


  METHOD BSRMATSET_CREATE_ENTITY.

    DATA: ls_mat       TYPE zcl_zbsr_form_mpc_ext=>ts_bsrmat,
          lo_exception TYPE REF TO /iwbep/cx_mgw_tech_exception.
    CREATE OBJECT lo_exception.

    io_data_provider->read_entry_data(
   IMPORTING
   es_data = ls_mat ).

    IF NOT ls_mat-mfrpn IS INITIAL.
      TRANSLATE ls_mat-mfrpn TO UPPER CASE.
      SELECT SINGLE matnr FROM mara INTO ls_mat-matnr WHERE mfrpn EQ ls_mat-mfrpn.
      IF NOT sy-subrc IS INITIAL.
        lo_exception->get_msg_container( )->add_message(
       EXPORTING
         iv_msg_type     = 'E'
         iv_msg_id      = '00'
         iv_msg_number  = '123'
         iv_msg_text    = 'Material' && ls_mat-mfrpn && 'doesnt exist'  ).
*          IV_MSG_V1
        RAISE EXCEPTION lo_exception.
        RETURN.
      ENDIF.
      er_entity = ls_mat.
    ENDIF.

  ENDMETHOD.


  METHOD BSRSTATUSLOGSET_GET_ENTITY.
    READ TABLE it_key_tab INTO DATA(ls_key) INDEX 1.
    CHECK sy-subrc IS INITIAL.
    SELECT SINGLE * FROM zbsr_status_log INTO CORRESPONDING FIELDS OF er_entity WHERE req_id EQ ls_key-value.
  ENDMETHOD.


  METHOD BSRSTATUSLOGSET_GET_ENTITYSET.
    DATA ra_emp TYPE RANGE OF zempid.
    READ TABLE it_key_tab INTO DATA(ls_key) INDEX 1.
    CHECK sy-subrc IS INITIAL.
    SELECT * FROM zbsr_status_log INTO CORRESPONDING FIELDS OF TABLE et_entityset WHERE req_id EQ ls_key-value. "#EC CI_ALL_FIELDS_NEEDED
    SELECT * FROM zbucp_config INTO TABLE @DATA(lt_config) WHERE name EQ 'REQ_STATUS' AND type = 'DROP_DOWN' AND field5 EQ 'BSR'. "#EC CI_ALL_FIELDS_NEEDED
    IF NOT et_entityset IS INITIAL.
      SELECT bname, accnt FROM usr02 INTO TABLE @DATA(lt_usr02) FOR ALL ENTRIES IN @et_entityset WHERE bname = @et_entityset-zwho.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_usr02 INTO DATA(ls_usr02).
          APPEND INITIAL LINE TO ra_emp ASSIGNING FIELD-SYMBOL(<fs_emp>).
          <fs_emp>-sign    = 'I'.
          <fs_emp>-option  = 'EQ'.
          <fs_emp>-low     = ls_usr02-accnt.
        ENDLOOP.
        SELECT employee_id, sapusername, displayname FROM zworkday_data INTO TABLE @DATA(lt_workday) WHERE employee_id IN @ra_emp.
      ENDIF.
    ENDIF.
    LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<fs_es>).
      IF <fs_es>-zwho IS ASSIGNED.
        CLEAR ls_usr02.
        READ TABLE lt_usr02 INTO ls_usr02 WITH KEY bname = <fs_es>-zwho.
        IF sy-subrc IS INITIAL.
          READ TABLE lt_workday INTO DATA(ls_workday) WITH KEY employee_id = ls_usr02-accnt.
          IF sy-subrc IS INITIAL.
            <fs_es>-name = ls_workday-displayname.
          ENDIF.

        ENDIF.
      ENDIF.
      IF <fs_es>-new_status IS ASSIGNED.
        READ TABLE lt_config INTO DATA(ls_config) WITH KEY field1 = <fs_es>-new_status.
        IF sy-subrc IS INITIAL.
          <fs_es>-new_status_txt = ls_config-field2.
        ENDIF.
      ENDIF.
      IF <fs_es>-old_status IS ASSIGNED.
        READ TABLE lt_config INTO ls_config WITH KEY field1 = <fs_es>-old_status.
        IF sy-subrc IS INITIAL.
          <fs_es>-old_status_txt = ls_config-field2.
        ENDIF.
      ENDIF.
      CLEAR: ls_workday, ls_config.
    ENDLOOP.
  ENDMETHOD.


  method BSRTESTMATSET_GET_ENTITYSET.

* DATA lt_stpox type TABLE OF stpox.
*    DATA lt_matcat TYPE TABLE OF cscmat.


*    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
*     EXPORTING
*       CAPID                       = 'ZP01'
*       DATUV                       = SY-DATUM
*       MEHRS                       = 'X'
*       MTNRV                       = 'mat1'
*       STLAN                       = '1'
*       WERKS                       = '4900'
*     TABLES
*          stb                   = lt_stpox
*          matcat                = lt_matcat
*     EXCEPTIONS
*       ALT_NOT_FOUND               = 1
*       CALL_INVALID                = 2
*       MATERIAL_NOT_FOUND          = 3
*       MISSING_AUTHORIZATION       = 4
*       NO_BOM_FOUND                = 5
*       NO_PLANT_DATA               = 6
*       NO_SUITABLE_BOM_FOUND       = 7
*       CONVERSION_ERROR            = 8
*       OTHERS                      = 9
*              .
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
  endmethod.


  METHOD DISCARDSET_CREATE_ENTITY.
    DATA lt_mailto TYPE bcsy_smtpa.
    io_data_provider->read_entry_data(
 IMPORTING
 es_data = er_entity ).
    SELECT SINGLE * FROM zbsr_header INTO @DATA(ls_header) WHERE req_id = @er_entity-req_id.
    IF sy-subrc IS INITIAL.
      ls_header-req_status = er_entity-req_status.
      MODIFY zbsr_header FROM ls_header.
      SELECT * FROM zbsr_status_log INTO TABLE @DATA(lt_bsr_log) WHERE req_id EQ @ls_header-req_id.
      IF sy-subrc IS INITIAL.
        SORT lt_bsr_log BY zwhen DESCENDING.
        READ TABLE lt_bsr_log INTO DATA(ls_bsr_log) INDEX 1.
        ls_bsr_log-old_status = ls_bsr_log-new_status.
        ls_bsr_log-new_status = er_entity-req_status.
        GET TIME STAMP FIELD ls_bsr_log-zwhen.
        ls_bsr_log-zwho       = sy-uname.
        MODIFY zbsr_status_log FROM ls_bsr_log.

        SELECT * FROM zbsr_notifcation INTO TABLE @DATA(lt_notif) WHERE req_id EQ @ls_header-req_id.
        LOOP AT lt_notif ASSIGNING FIELD-SYMBOL(<fs_notif>).
          APPEND INITIAL LINE TO lt_mailto ASSIGNING FIELD-SYMBOL(<fs_email>).
          <fs_email> = <fs_notif>-email.
        ENDLOOP.
        zcl_bsr_app_utility=>notifications(
          EXPORTING
            p_req_id     = ls_header-req_id   " Request ID
            p_req_status = '99'
            t_mail_to    = lt_mailto ).   " Request Status

      ENDIF.
    ENDIF.
  ENDMETHOD.


  method DISCARDSET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->DISCARDSET_GET_ENTITY
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
    er_entity = er_entity .

  endmethod.


  METHOD DISCARDSET_GET_ENTITYSET.
    READ TABLE it_key_tab  INTO DATA(ls_key) INDEX 1.
    IF sy-subrc IS INITIAL.
      SELECT SINGLE * FROM zbsr_header INTO @DATA(ls_header) WHERE req_id = @ls_key-value.  "#EC CI_ALL_FIELDS_NEEDED
      IF sy-subrc IS INITIAL.
        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs>).
        MOVE-CORRESPONDING ls_header TO <fs>.
      ENDIF.
*    et_entityset = et_entityset .
    ENDIF.

  ENDMETHOD.


  METHOD DISCARDSET_UPDATE_ENTITY.
**TRY.
*CALL METHOD SUPER->DISCARDSET_UPDATE_ENTITY
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


    io_data_provider->read_entry_data(
 IMPORTING
 es_data = er_entity ).
    SELECT SINGLE * FROM zbsr_header INTO @DATA(ls_header) WHERE req_id = @er_entity-req_id.
    IF sy-subrc IS INITIAL.
      ls_header-req_status = er_entity-req_status.
      MODIFY zbsr_header FROM ls_header .


    ENDIF.
  ENDMETHOD.


  method PLANTSHSET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->PLANTSHSET_GET_ENTITY
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


  METHOD PLANTSHSET_GET_ENTITYSET.
*    DATA: lt_entityset  TYPE zcl_zbsr_form_mpc_ext=>tt_plantsh.
*
*    cl_fkk_shlp_processor=>get_values(
*           EXPORTING
*             iv_search_help               = 'H_T001W'
*             iv_search_help_field         = 'WERKS'
*             io_tech_request_context      = io_tech_request_context
*           IMPORTING
*             et_entityset                 = lt_entityset
*             es_response_context          = es_response_context ).
*
*    et_entityset = lt_entityset .


  ENDMETHOD.


  METHOD READ_MAT_CHAR_IDENTIFIER.

    DATA: lt_num    TYPE TABLE OF bapi1003_alloc_values_num,
          lt_char   TYPE TABLE OF bapi1003_alloc_values_char,
          lt_scurr  TYPE TABLE OF bapi1003_alloc_values_curr,
          lt_return TYPE bapiret2_t,
          lv_matnr  TYPE bapi1003_key-object.
    CLEAR lv_matnr.
    lv_matnr = iv_matnr.
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_matnr
        objecttable     = 'MARA'
        classnum        = 'IC_CHARACTERISTICS'
        classtype       = '001'
      TABLES
        allocvaluesnum  = lt_num
        allocvalueschar = lt_char
        allocvaluescurr = lt_scurr
        return          = lt_return.
    READ TABLE lt_char INTO DATA(ls_char) WITH KEY charact = 'IDENTIFIER'.
    IF sy-subrc IS INITIAL.
      CLEAR rs_char.
      rs_char = ls_char.
    ELSE.
      REFRESH lt_char[].
      CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
        EXPORTING
          objectkey       = lv_matnr
          objecttable     = 'MARA'
          classnum        = 'ZTST_MAT_CLASS'
          classtype       = '001'
        TABLES
          allocvaluesnum  = lt_num
          allocvalueschar = lt_char
          allocvaluescurr = lt_scurr
          return          = lt_return.
      READ TABLE lt_char INTO ls_char WITH KEY charact = 'IDENTIFIER'.
      IF sy-subrc EQ 0.
        CLEAR rs_char.
        rs_char = ls_char.
      ENDIF.
    ENDIF.

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
          text = text && ls_line-tdline.
        ELSE.
          text = ls_line-tdline.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD READ_TEXT_FORM.

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
          text = text && ls_line-tdline.
        ELSE.
          text = ls_line-tdline.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD SAVE_TEXT.

    DATA ls_thead       TYPE thead.
    DATA lt_lines       TYPE TABLE OF tline.

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
    REFRESH lt_lines.
    CALL FUNCTION 'VB_CP_CONVERT_STRING_2_ITF'
      EXPORTING
        i_string = text
      TABLES
        et_table = lt_lines.

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
* EXCEPTIONS
*       ID              = 1
*       LANGUAGE        = 2
*       NAME            = 3
*       OBJECT          = 4
*       OTHERS          = 5
      .
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'COMMIT_TEXT'
        EXPORTING
          savemode_direct = abap_true.
    ELSE.
      ev_failed = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
