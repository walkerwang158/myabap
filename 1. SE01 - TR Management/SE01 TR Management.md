```abap
*& Report ZSE01
*& Description 请求管理
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zse01.

TABLES e070v.

DATA gt_detail TYPE TABLE OF e071.

TYPES: BEGIN OF ty_e070v,
         check,
         trkorr     TYPE e070v-trkorr,
         trfunction TYPE e070v-trfunction,
         trstatus   TYPE e070v-trstatus,
         as4user    TYPE e070v-as4user,
         as4date    TYPE e070v-as4date,
         as4text    TYPE e070v-as4text,
         relstat    TYPE c LENGTH 4, "释放状态
         zcstms     TYPE c LENGTH 4, "生成副本请求
         atcstat    TYPE c LENGTH 4, "ATC检查（双击检查）
         tmsstat    TYPE c LENGTH 4, "传输检查（双击检查）
         codstat    TYPE c LENGTH 4, "代码度量（双击检查）
         othstat    TYPE c LENGTH 4, "其他检查
         detail     LIKE gt_detail,
       END OF ty_e070v.
DATA: gt_e070v TYPE TABLE OF ty_e070v.
DATA gr_prog TYPE RANGE OF rpy_prog-progname .
DATA gr_fugr TYPE RANGE OF tlibg-area.
DATA gr_func TYPE RANGE OF tfdir-funcname.
DATA gr_clas TYPE RANGE OF seoclasstx-clsname.
DATA gr_trs TYPE RANGE OF e070v-trkorr.

DATA: go_salv_table TYPE REF TO cl_salv_table.

CONSTANTS cos_date_create  TYPE datum VALUE '20260101'.  "ATC检查开始日期
CONSTANTS cos_date_cfg     TYPE datum VALUE '20260101'.  "配置表变更信息检查开始日期
CONSTANTS cos_target       TYPE rfcdes-rfcdest VALUE 'PRDSOLMANCHECK'. "请求传输检查RFC Destination
CONSTANTS cos_devcl        TYPE devclass VALUE 'Z*'. "开发包

*Event Definition
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS on_double_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.
    CLASS-METHODS on_single_click
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row column.
ENDCLASS.

*Event Implementation
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_double_click.
    "on_double_click
    PERFORM frm_double_click USING row column.
  ENDMETHOD.

  METHOD on_single_click.
    "on_single_click
    PERFORM frm_single_click USING row column.
  ENDMETHOD.
ENDCLASS.

SELECT-OPTIONS: s_tr FOR e070v-trkorr.
SELECT-OPTIONS: s_chur FOR e070v-as4user.
SELECT-OPTIONS: s_chdt FOR e070v-as4date.
PARAMETERS: p_case TYPE char10. "Change Number
PARAMETERS: p_dev AS CHECKBOX USER-COMMAND trs.
PARAMETERS: p_conf AS CHECKBOX USER-COMMAND trs.
PARAMETERS: p_copy AS CHECKBOX USER-COMMAND trs.
PARAMETERS: p_disp AS CHECKBOX.
PARAMETERS: p_deve AS CHECKBOX.
PARAMETERS: p_rele AS CHECKBOX.

INITIALIZATION.
  %_s_tr_%_app_%-text   = '请求号'.
  %_s_chur_%_app_%-text = '上次更改者'.
  %_s_chdt_%_app_%-text = '上次更改日期'.
  %_p_case_%_app_%-text = '变更号'.
  %_p_dev_%_app_%-text  = '工作台请求'.
  %_p_conf_%_app_%-text = '配置请求'.
  %_p_copy_%_app_%-text = '副本请求'.
  %_p_disp_%_app_%-text = '只显示请求'.
  %_p_deve_%_app_%-text = '开发中请求'.
  %_p_rele_%_app_%-text = '已释放请求'.
  p_dev = 'X'.
  p_conf = 'X'.
  p_copy = ' '.
  p_disp = 'X'.
  p_deve = 'X'.
  p_rele = ' '.
  APPEND VALUE #( low = sy-uname ) TO s_chur.
  APPEND VALUE #( low = sy-datum - 60 high = sy-datum ) TO s_chdt.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tr-low.
  PERFORM frm_f4_trkorr CHANGING s_tr-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tr-high.
  PERFORM frm_f4_trkorr CHANGING s_tr-high.

START-OF-SELECTION.
  PERFORM frm_get_trs.

END-OF-SELECTION.
  PERFORM frm_display_trs.

*&---------------------------------------------------------------------*
*& Form frm_f4_trkorr
*&---------------------------------------------------------------------*
FORM frm_f4_trkorr CHANGING cv_trkorr.

  DATA: lt_e071  TYPE TABLE OF e071,
        lt_e071k TYPE TABLE OF e071k,
        lv_task  TYPE trkorr.
  DATA: lv_order_type TYPE  trfunction,
        lv_task_type  TYPE trfunction,
        lv_category   TYPE trcateg.

  IF p_dev IS NOT INITIAL.
    lv_order_type = 'K'.
    lv_task_type  = 'S'.
  ELSEIF p_conf IS NOT INITIAL.
    lv_order_type = 'W'.
    lv_task_type  = 'Q'.
    lv_category = 'CUST'.
  ENDIF.

  CALL FUNCTION 'TRINT_ORDER_CHOICE'
    EXPORTING
      wi_order_type = lv_order_type
      wi_task_type  = lv_task_type
      wi_category   = lv_category
    IMPORTING
      we_order      = cv_trkorr
      we_task       = lv_task
    TABLES
      wt_e071       = lt_e071
      wt_e071k      = lt_e071k
    EXCEPTIONS
      OTHERS        = 1.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_get_trs
*&---------------------------------------------------------------------*
FORM frm_get_trs .

  DATA lr_trfunction    TYPE RANGE OF e070v-trfunction.
  DATA lr_trstatus      TYPE RANGE OF e070v-trstatus.
  DATA ls_request_header TYPE trwbo_request_header.
  DATA lt_objects        TYPE tr_objects.
  DATA lt_e071           TYPE tt_e071.
  DATA lt_source TYPE TABLE OF string.

  IF p_dev = 'X'.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'K' ) TO lr_trfunction.
  ENDIF.
  IF p_conf = 'X'.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'W' ) TO lr_trfunction.
  ENDIF.
  IF p_copy = 'X'.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'T' ) TO lr_trfunction.
  ENDIF.
  IF p_deve = 'X'.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'D' ) TO lr_trstatus.
  ENDIF.
  IF p_rele = 'X'.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'R' ) TO lr_trstatus.
  ENDIF.

  IF p_case IS NOT INITIAL.
    DATA(lv_text) = |%{ p_case }%|.
  ELSE.
    lv_text = |%|.
  ENDIF.

  SELECT ' ' AS check, trkorr, trfunction, trstatus, as4user, as4date, as4text
    FROM e070v AS v
    WHERE v~trkorr IN @s_tr
      AND v~as4user IN @s_chur
      AND v~as4date IN @s_chdt
      AND v~trfunction IN @lr_trfunction
      AND v~trstatus IN @lr_trstatus
      AND v~as4text LIKE @lv_text
    INTO CORRESPONDING FIELDS OF TABLE @gt_e070v.

  SORT gt_e070v BY trkorr DESCENDING.

  LOOP AT gt_e070v ASSIGNING FIELD-SYMBOL(<fs_e070v>).
    IF <fs_e070v>-trstatus = 'R'.
      <fs_e070v>-relstat = icon_green_light.
    ELSE.
      <fs_e070v>-relstat = icon_red_light.
    ENDIF.
    <fs_e070v>-zcstms  = icon_create.
    <fs_e070v>-atcstat = icon_yellow_light.
    <fs_e070v>-tmsstat = icon_yellow_light.
    <fs_e070v>-codstat = icon_green_light.

    " Get objects in TR
    CLEAR: ls_request_header, lt_objects.
    ls_request_header-trkorr = <fs_e070v>-trkorr.
    CALL FUNCTION 'TR_GET_OBJECTS_OF_REQ_AN_TASKS'
      EXPORTING
        is_request_header = ls_request_header
      IMPORTING
        et_objects        = lt_objects
      EXCEPTIONS
        invalid_input     = 1
        OTHERS            = 2.

    " Get r3tr keys
    TRY.
        DATA(lo_converter) = cl_satc_ac_r3tr_key_service=>create_r3tr_key_service(  ).
        lo_converter->convert_transport_to_r3tr_keys(
          EXPORTING
            i_transport_objects = lt_objects
          IMPORTING
            e_object_keys       = DATA(lt_result) ).
      CATCH cx_satc_root INTO DATA(failure).
    ENDTRY.
*    cl_satc_ac_program_name_svc=>add_main_programs_4_includes( CHANGING ch_keys = lt_result ).

    " 其他检查
    <fs_e070v>-othstat = icon_green_light.

    " 存在Enhancement SPOT/Classical BADI/CMOD增强
    IF line_exists( lt_result[ obj_type = `ENHO` ] ) OR
       line_exists( lt_result[ obj_type = `SXCI` ] ) OR
       line_exists( lt_result[ obj_type = `CMOD` ] ) OR
       line_exists( lt_result[ obj_type = `FUGS` ] ) OR
       line_exists( lt_result[ obj_type = `PROG` obj_name(2) = `ZX` ] ).
      lt_e071 = VALUE #( FOR ls_wa IN lt_result WHERE ( obj_type = `ENHO`
                                                      OR obj_type = `SXCI`
                                                      OR obj_type = `CMOD`
                                                      OR obj_type = `FUGS`
                                                      OR ( obj_type = `PROG` AND obj_name(2) = `ZX` ) )
                            ( CORRESPONDING #( ls_wa MAPPING object = obj_type ) ) ).
    ENDIF.

    LOOP AT lt_result INTO DATA(ls_result) WHERE obj_type = `CLAS`.
      SELECT SINGLE clsname
        FROM seometarel
        WHERE clsname = @ls_result-obj_name AND refclsname = 'IF_BADI_INTERFACE'
        INTO @DATA(lv_badi).
      IF sy-subrc <> 0.
        SELECT SINGLE imp_class
          FROM sxc_class
          WHERE imp_class = @ls_result-obj_name
          INTO @lv_badi.
      ENDIF.
      IF sy-subrc = 0 AND lv_badi IS NOT INITIAL.
        APPEND CORRESPONDING e071( ls_result MAPPING object = obj_type ) TO lt_e071.
      ENDIF.
    ENDLOOP.

    IF lt_e071 IS NOT INITIAL.
      MODIFY lt_e071 FROM VALUE #( activity = '存在增强' ) TRANSPORTING activity WHERE activity IS INITIAL.
      <fs_e070v>-othstat = icon_red_light.
      <fs_e070v>-detail  = CORRESPONDING #( BASE ( <fs_e070v>-detail ) lt_e071 ).
      CLEAR lt_e071.
    ENDIF.

    " 存在SAP修改
    " Customer system => Check SAP objects from list of c_r3tr_keys
    DATA(lo_ns_reg) = cl_satc_ac_namespace_registry=>get_registry( ).
    LOOP AT lt_result INTO ls_result WHERE obj_type = 'PROG'
                                              OR obj_type = 'CLAS'
                                              OR obj_type = 'FUGR'
                                              OR obj_type = 'FUGS'. "Only evaluates these object types
      IF ( lo_ns_reg->is_sap_object( i_object_type = ls_result-obj_type i_object_name = ls_result-obj_name  ) = abap_true ).
        APPEND CORRESPONDING e071( ls_result MAPPING object = obj_type ) TO lt_e071.
      ENDIF.
    ENDLOOP.
    IF lt_e071 IS NOT INITIAL.
      MODIFY lt_e071 FROM VALUE #( activity = '存在SAP修改' ) TRANSPORTING activity WHERE activity IS INITIAL.
      <fs_e070v>-othstat = icon_red_light.
      <fs_e070v>-detail  = CORRESPONDING #( BASE ( <fs_e070v>-detail ) lt_e071 ).
      CLEAR lt_e071.
    ENDIF.

    " 存在更改日志
    DATA(lv_string) = <fs_e070v>-as4text.
    CALL FUNCTION 'PREPARE_STRING'
      EXPORTING
        i_valid_chars = '1234567890 '
      CHANGING
        c_string      = lv_string.
    SHIFT lv_string LEFT DELETING LEADING space.
    SPLIT lv_string AT space INTO lv_string DATA(lv_dummy).
    LOOP AT lt_result INTO ls_result WHERE obj_type = 'PROG'
                                              OR obj_type = 'CLAS'
                                              OR obj_type = 'FUGR'
                                              OR obj_type = 'FUGS'. "Only evaluates these object types
      DATA(lv_obj_name) = ls_result-obj_name.
      DATA(lv_cdlstat) = icon_green_light.
      CASE ls_result-obj_type.
        WHEN 'PROG'.
          SELECT COUNT(*)
            FROM trdir
            WHERE name = lv_obj_name
              AND subc = '1'.
          CHECK sy-subrc = 0.
        WHEN 'CLAS'.
          CONTINUE.
        WHEN 'FUGR'.
          CONTINUE.
        WHEN 'FUGS'.
          CONTINUE.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      PERFORM frm_check_trkorr USING lv_obj_name <fs_e070v>-trkorr lv_string CHANGING lv_cdlstat.
      IF lv_cdlstat = icon_red_light.
        APPEND CORRESPONDING e071( ls_result MAPPING object = obj_type ) TO lt_e071.
      ENDIF.

    ENDLOOP.
    LOOP AT lt_objects INTO DATA(ls_object) WHERE object = 'PROG'
                                              OR object = 'REPS'
                                              OR object = 'CLAS'
                                              OR object = 'FUGR'
                                              OR object = 'FUGS'
                                              OR object = 'METH'
                                              OR object = 'FUNC'. "Only evaluates these object types
      DATA(lv_obj_name2) = ls_object-obj_name.
      lv_cdlstat = icon_green_light.
      CASE ls_object-object.
        WHEN 'PROG'.
          CHECK lv_obj_name2(2) = 'ZX'.
        WHEN 'REPS'.
          CONTINUE.
        WHEN 'CLAS'.
          CLASS cl_oo_include_naming DEFINITION LOAD.
          DATA lo_oref TYPE REF TO if_oo_class_incl_naming.
          "Skip CL_PROXY_CLIENT
          SELECT SINGLE clsname
            FROM seometarel
            WHERE clsname = @lv_obj_name2 AND refclsname = 'CL_PROXY_CLIENT'
            INTO @DATA(lv_proxy).
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
          DATA(lv_cifkey) = CONV seoclskey( lv_obj_name2 ).
          cl_oo_include_naming=>get_instance_by_cifkey( EXPORTING  cifkey         = lv_cifkey
                                                        RECEIVING  cifref         = DATA(lo_cifref)
                                                        EXCEPTIONS no_objecttype  = 1
                                                                   internal_error = 2
                                                                   OTHERS         = 3 ).
          lo_oref ?= lo_cifref.
          CHECK sy-subrc = 0.
          DATA(lt_mtds_w_incl) = lo_oref->get_all_method_includes( ).
          DATA(lv_obj_name3) = lv_obj_name2.
          LOOP AT lt_mtds_w_incl INTO DATA(ls_mtds_w_incl).
            lv_cdlstat = icon_green_light.
            lv_obj_name3 = |{ ls_mtds_w_incl-incname }|.
            PERFORM frm_check_trkorr USING lv_obj_name3 <fs_e070v>-trkorr lv_string CHANGING lv_cdlstat.
            IF lv_cdlstat = icon_red_light.
              APPEND INITIAL LINE TO lt_e071 ASSIGNING FIELD-SYMBOL(<fs_e071>).
              <fs_e071>-object = 'METH'.
              <fs_e071>-obj_name = ls_mtds_w_incl-cpdkey.
            ENDIF.
          ENDLOOP.
          CONTINUE.
        WHEN 'FUGR'.
          lv_obj_name3 = |SAPL{ lv_obj_name2 }|.
          SELECT * FROM tfdir WHERE pname = @lv_obj_name3 AND funcname LIKE 'Z%' INTO TABLE @DATA(lt_tfdir).
          LOOP AT lt_tfdir INTO DATA(ls_tfdir).
            lv_cdlstat = icon_green_light.
            lv_obj_name3 = |{ ls_tfdir-pname+3 }U{ ls_tfdir-include }|.
            PERFORM frm_check_trkorr USING lv_obj_name3 <fs_e070v>-trkorr lv_string CHANGING lv_cdlstat.
            IF lv_cdlstat = icon_red_light.
              APPEND INITIAL LINE TO lt_e071 ASSIGNING <fs_e071>.
              <fs_e071>-object = 'FUNC'.
              <fs_e071>-obj_name = ls_tfdir-funcname.
            ENDIF.
          ENDLOOP.
          CLEAR lt_tfdir.
          CONTINUE.
        WHEN 'FUGS'.
          CONTINUE.
        WHEN 'METH'.
          DATA(lv_class) = lv_obj_name2(30).
          DATA(lv_method) = lv_obj_name2+30.
          "Skip CL_PROXY_CLIENT
          SELECT SINGLE clsname
            FROM seometarel
            WHERE clsname = @lv_class AND refclsname = 'CL_PROXY_CLIENT'
            INTO @lv_proxy.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
          lv_cifkey = CONV seoclskey( lv_class ).
          cl_oo_include_naming=>get_instance_by_cifkey( EXPORTING  cifkey         = lv_cifkey
                                                        RECEIVING  cifref         = lo_cifref
                                                        EXCEPTIONS no_objecttype  = 1
                                                                   internal_error = 2
                                                                   OTHERS         = 3 ).
          lo_oref ?= lo_cifref.
          CHECK sy-subrc = 0.
          lt_mtds_w_incl = lo_oref->get_all_method_includes( ).
          LOOP AT lt_mtds_w_incl INTO ls_mtds_w_incl WHERE cpdkey-cpdname = lv_method.
            EXIT.
          ENDLOOP.
          CHECK sy-subrc = 0.
          lv_obj_name2 = ls_mtds_w_incl-incname.
        WHEN 'FUNC'.
          SELECT SINGLE concat( pname, concat( 'U', include ) ) AS obj_name
            FROM tfdir
            WHERE funcname = @lv_obj_name2
            INTO @lv_obj_name2.
          CHECK sy-subrc = 0.
          lv_obj_name2 = lv_obj_name2+3.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      PERFORM frm_check_trkorr USING lv_obj_name2 <fs_e070v>-trkorr lv_string CHANGING lv_cdlstat.
      IF lv_cdlstat = icon_red_light.
        APPEND CORRESPONDING e071( ls_object ) TO lt_e071.
      ENDIF.

    ENDLOOP.
    IF lt_e071 IS NOT INITIAL.
      SORT lt_e071 BY object obj_name.
      DELETE ADJACENT DUPLICATES FROM lt_e071 COMPARING object obj_name.
      MODIFY lt_e071 FROM VALUE #( activity = '缺少变更日志' ) TRANSPORTING activity WHERE activity IS INITIAL.
      <fs_e070v>-othstat = icon_red_light.
      <fs_e070v>-detail  = CORRESPONDING #( BASE ( <fs_e070v>-detail ) lt_e071 ).
      CLEAR lt_e071.
    ENDIF.

    " 存在事务码，检查是否复合规则
    LOOP AT lt_result INTO ls_result WHERE obj_type = 'TRAN'.
      IF NOT contains( val = ls_result-obj_name regex = `^Z\u{2}\d{3}\D*$` ).
        APPEND CORRESPONDING e071( ls_result MAPPING object = obj_type ) TO lt_e071.
      ENDIF.
    ENDLOOP.
    IF lt_e071 IS NOT INITIAL.
      MODIFY lt_e071 FROM VALUE #( activity = '事务码不符合规则^Z\u{2}\d{3}\D*$' ) TRANSPORTING activity WHERE activity IS INITIAL.
      <fs_e070v>-othstat = icon_red_light.
      <fs_e070v>-detail  = CORRESPONDING #( BASE ( <fs_e070v>-detail ) lt_e071 ).
      CLEAR lt_e071.
    ENDIF.

    " 存在维护视图，检查是否添加创建、修改信息，是否存在01保存前的事件
    LOOP AT lt_result INTO ls_result WHERE obj_type = 'VIEW' OR obj_type = 'TABL'.
      " 维护视图或表
      SELECT COUNT(*)
        FROM tvdir
        WHERE tabname = ls_result-obj_name.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      " 主表
      SELECT SINGLE roottab
        FROM dd25l
        WHERE viewname = @ls_result-obj_name
        INTO @DATA(lv_tabnam).
      IF sy-subrc <> 0.
        lv_tabnam = ls_result-obj_name.
      ENDIF.

      " 配置表创建日期
      SELECT COUNT(*)
        FROM tadir
        WHERE object = 'TABL'
          AND obj_name = lv_tabnam
          AND created_on GE cos_date_cfg.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      " 存在创建、修改信息
      SELECT COUNT(*)
        FROM dd03l
        WHERE tabname = @lv_tabnam
          AND fieldname = '.INCLUDE'
          AND ( precfield = 'ZCCAS_CREATE_INFO' OR precfield = 'ZCCAS_CHANGE_INFO' )
        INTO @DATA(lv_count).
      IF lv_count <> 2.
        APPEND CORRESPONDING e071( ls_result MAPPING object = obj_type ) TO lt_e071.
        CONTINUE.
      ENDIF.

      " 存在01保存前的事件
      SELECT COUNT(*)
        FROM tvimf
        WHERE tabname = ls_result-obj_name
          AND event = '01'.
      IF sy-subrc <> 0.
        APPEND CORRESPONDING e071( ls_result MAPPING object = obj_type ) TO lt_e071.
        CONTINUE.
      ENDIF.

    ENDLOOP.
    IF lt_e071 IS NOT INITIAL.
      MODIFY lt_e071 FROM VALUE #( activity = '配置表不存在创建和修改字段，或者缺少保存事件' ) TRANSPORTING activity WHERE activity IS INITIAL.
      <fs_e070v>-othstat = icon_red_light.
      <fs_e070v>-detail  = CORRESPONDING #( BASE ( <fs_e070v>-detail ) lt_e071 ).
      CLEAR lt_e071.
    ENDIF.

    " 存在标准屏幕增强
    LOOP AT lt_objects INTO ls_object WHERE object = 'DYNP' AND obj_name(1) <> 'Z' AND obj_name(5) <> 'SAPLZ' AND obj_name(5) <> 'SAPMZ'.
      APPEND CORRESPONDING e071( ls_object ) TO lt_e071.
    ENDLOOP.
    IF lt_e071 IS NOT INITIAL.
      MODIFY lt_e071 FROM VALUE #( activity = '存在标准屏幕增强' ) TRANSPORTING activity WHERE activity IS INITIAL.
      <fs_e070v>-othstat = icon_red_light.
      <fs_e070v>-detail  = CORRESPONDING #( BASE ( <fs_e070v>-detail ) lt_e071 ).
      CLEAR lt_e071.
    ENDIF.

    " 存在标准表增强
    LOOP AT lt_result INTO ls_result WHERE obj_type = 'TABL'.
      IF ls_result-obj_name(1) <> 'Z'.
        APPEND CORRESPONDING e071( ls_result MAPPING object = obj_type ) TO lt_e071.
      ELSE.
        SELECT SINGLE sqltab
          FROM dd02l
          WHERE tabname = @ls_result-obj_name
            AND tabclass = 'APPEND'
          INTO @DATA(lv_sqltab).
        IF lv_sqltab IS NOT INITIAL AND lv_sqltab(1) <> 'Z'.
          CLEAR lv_sqltab.
          APPEND CORRESPONDING e071( ls_result MAPPING object = obj_type ) TO lt_e071.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF lt_e071 IS NOT INITIAL.
      MODIFY lt_e071 FROM VALUE #( activity = '存在标准表增强' ) TRANSPORTING activity WHERE activity IS INITIAL.
      <fs_e070v>-othstat = icon_red_light.
      <fs_e070v>-detail  = CORRESPONDING #( BASE ( <fs_e070v>-detail ) lt_e071 ).
      CLEAR lt_e071.
    ENDIF.

*   " 其他
*   LOOP AT lt_result INTO ls_result.
*     " 单行代码是否超过72个字符
*     CASE ls_result-obj_type.
*        WHEN 'PROG' OR 'REPS'.
*          CLEAR lt_source.
*          READ REPORT ls_result-obj_name INTO lt_source.
*          IF sy-subrc = 0.
*            LOOP AT lt_source ASSIGNING FIELD-SYMBOL(<fs_source>).
*              CONDENSE <fs_source>.
*              CHECK <fs_source> IS NOT INITIAL.
*              IF <fs_source>(1) = '*' OR <fs_source>(1) = '"'.
*                CONTINUE.
*              ENDIF.
*              IF strlen( <fs_source> ) > 72.
*                DATA(lv_72) = 'X'.
*                EXIT.
*              ENDIF.
*            ENDLOOP.
*            IF lv_72 = 'X'.
*              <fs_e070v>-othstat = icon_red_light.
*              APPEND CORRESPONDING e071( ls_result MAPPING object = obj_type ) TO lt_e071 ASSIGNING <fs_e071>.
*              <fs_e071>-activity = '单行代码超过72个字符'.
*              <fs_e070v>-detail  = VALUE #( base ( <fs_e070v>-detail ) CORRESPONDING lt_e071 ).
*              clear lt_e071.
*            ENDIF.
*          ENDIF.
*       WHEN OTHERS.
*     ENDCASE.
*   ENDLOOP.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_display_trs
*&---------------------------------------------------------------------*
FORM frm_display_trs .

*&-> SALV Table
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = go_salv_table
        CHANGING
          t_table      = gt_e070v.
    CATCH cx_salv_msg .
      RETURN.
  ENDTRY.

*&-> Display
  go_salv_table->get_functions( )->set_all( abap_true ).
  go_salv_table->get_layout( )->set_default( abap_true ).
  go_salv_table->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).
  go_salv_table->get_display_settings( )->set_striped_pattern( abap_true ).
  go_salv_table->get_display_settings( )->set_list_header( |{ sy-title }, Total: { lines( gt_e070v ) }| ).
  go_salv_table->get_columns( )->set_optimize( abap_true ).
  go_salv_table->get_columns( )->set_key_fixation( abap_true ).
  go_salv_table->get_columns( )->get_column( 'TRFUNCTION' )->set_ddic_reference( VALUE salv_s_ddic_reference( table = 'E070V' field = 'TRFUNCTION' ) ).
  go_salv_table->get_columns( )->get_column( 'TRSTATUS' )->set_ddic_reference( VALUE salv_s_ddic_reference( table = 'E070V' field = 'TRSTATUS' ) ).
  DATA: lo_column TYPE REF TO cl_salv_column_table.
  lo_column ?= go_salv_table->get_columns( )->get_column( 'RELSTAT' ).
  lo_column->set_medium_text( '释放状态' ).
  lo_column->set_icon( if_salv_c_bool_sap=>true ).
  lo_column ?= go_salv_table->get_columns( )->get_column( 'ZCSTMS' ).
  lo_column->set_medium_text( '生成副本请求' ).
  lo_column->set_icon( if_salv_c_bool_sap=>true ).
  lo_column ?= go_salv_table->get_columns( )->get_column( 'ATCSTAT' ).
  lo_column->set_medium_text( 'ATC检查（双击检查）' ).
  lo_column->set_icon( if_salv_c_bool_sap=>true ).
  lo_column ?= go_salv_table->get_columns( )->get_column( 'TMSSTAT' ).
  lo_column->set_medium_text( '传输检查（双击检查）' ).
  lo_column->set_icon( if_salv_c_bool_sap=>true ).
  lo_column ?= go_salv_table->get_columns( )->get_column( 'CODSTAT' ).
  lo_column->set_medium_text( '代码度量（双击检查）' ).
  lo_column->set_icon( if_salv_c_bool_sap=>true ).
  lo_column ?= go_salv_table->get_columns( )->get_column( 'OTHSTAT' ).
  lo_column->set_medium_text( '其他检查' ).
  lo_column->set_icon( if_salv_c_bool_sap=>true ).
  lo_column ?= go_salv_table->get_columns( )->get_column( 'CHECK' ).
  lo_column->set_medium_text( '选择' ).
  lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
  DATA(lo_event) = go_salv_table->get_event( ).
  SET HANDLER lcl_event_handler=>on_double_click FOR lo_event.
  SET HANDLER lcl_event_handler=>on_single_click FOR lo_event.
  go_salv_table->get_sorts( )->add_sort(
                                 columnname = 'TRFUNCTION'
                                 sequence   = if_salv_c_sort=>sort_up ).
  go_salv_table->get_sorts( )->add_sort(
                                 columnname = 'TRSTATUS'
                                 sequence   = if_salv_c_sort=>sort_up ).
  go_salv_table->get_sorts( )->add_sort(
                                 columnname = 'AS4USER'
                                 sequence   = if_salv_c_sort=>sort_up ).
  go_salv_table->get_sorts( )->add_sort(
                                 columnname = 'AS4DATE'
                                 sequence   = if_salv_c_sort=>sort_down ).
  go_salv_table->get_sorts( )->add_sort(
                                 columnname = 'TRKORR'
                                 sequence   = if_salv_c_sort=>sort_down ).

  go_salv_table->display( ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_double_click
*&---------------------------------------------------------------------*
FORM frm_double_click  USING    pv_row  TYPE  salv_de_row
                                pv_column TYPE  salv_de_column.

  DATA lr_prog TYPE RANGE OF rpy_prog-progname .
  DATA lr_fugr TYPE RANGE OF tlibg-area.
  DATA lr_func TYPE RANGE OF tfdir-funcname.
  DATA lr_clas TYPE RANGE OF seoclasstx-clsname.
  DATA lr_fugr1 TYPE RANGE OF tlibg-area.
  DATA lr_trs TYPE RANGE OF e070v-trkorr.
  DATA lr_dc TYPE RANGE OF tadir-devclass.

  DATA lv_error_type TYPE trwbo_charflag.

  DATA lt_summary     TYPE trcheckresext_tab.
  DATA lo_slin_res    TYPE REF TO cl_ci_transport_check.
  DATA lv_atc_res     TYPE string.
  DATA lt_docu_res    TYPE trdocuresults.
  DATA lt_pack_res    TYPE paddcheckres.
  DATA lt_gtabkey_res TYPE gtabkeyerror.
  DATA lo_atc_res     TYPE REF TO if_transport_check_service.

  DATA lv_canceled_by_user TYPE abap_bool.

  READ TABLE gt_e070v INDEX pv_row INTO DATA(ls_e070v).

  CASE pv_column.
    WHEN 'TRKORR'.
      CALL FUNCTION 'TR_PRESENT_REQUEST'
        EXPORTING
          iv_trkorr   = ls_e070v-trkorr
          iv_showonly = p_disp.

    WHEN 'ZCSTMS '.
      SUBMIT zcstms "VIA SELECTION-SCREEN
        WITH p_trkorr = ls_e070v-trkorr
        WITH p_rel = abap_true
        AND RETURN.

    WHEN 'ATCSTAT'.
      CLEAR: lv_error_type,
             lt_summary,
             lo_slin_res,
             lv_atc_res,
             lt_docu_res,
             lt_pack_res,
             lt_gtabkey_res,
             lo_atc_res.
      DATA(lt_datum2) = VALUE trgr_date( ( sign = 'I' option = 'BT' low = cos_date_create high = sy-datum ) ).
      EXPORT atc_date = lt_datum2 TO MEMORY ID 'ZATC_DATE'.
      CALL FUNCTION 'TR_INSPECT_OBJECTS'
        EXPORTING
          iv_trkorr       = ls_e070v-trkorr
          iv_dialog       = ' '
        IMPORTING
          ev_error_type   = lv_error_type
        EXCEPTIONS
          invalid_request = 1
          OTHERS          = 2.
      CALL FUNCTION 'TR_GET_OBJ_INSPECTION_RESULTS'
        IMPORTING
          et_summary     = lt_summary
          eo_slin_res    = lo_slin_res
          ev_atc_res     = lv_atc_res
          et_docu_res    = lt_docu_res
          et_pack_res    = lt_pack_res
          et_gtabkey_res = lt_gtabkey_res
          eo_atc_res     = lo_atc_res.
      IF lo_atc_res IS BOUND.
        DATA lv_profile TYPE satc_d_ac_chk_profile_name.
        IMPORT atc_vart = lv_profile FROM MEMORY ID 'ZATC_PROFILE'.
        IF sy-subrc = 0 AND lv_profile = 'CMOC5'.
          EXPORT atc_variant = 'CMOC-ATC-TFM' TO MEMORY ID 'ZATC_VARIANT'.
        ELSE.
          EXPORT atc_variant = 'CMOC-ATC' TO MEMORY ID 'ZATC_VARIANT'.
        ENDIF.
        lo_atc_res->show( EXCEPTIONS OTHERS = 1 ).
      ELSE.
*CALL FUNCTION 'TR_DISP_OBJ_INSPECTION_RESULTS'
*  EXPORTING
*    it_summary          = lt_summary
*    io_slin_res         = lo_slin_res
*    io_atc_res          = lo_atc_res
*    it_docu_res         = lt_docu_res
*    it_pack_res         = lt_pack_res
*    it_gtabkey_res      = lt_gtabkey_res
*  IMPORTING
*    ev_canceled_by_user = lv_canceled_by_user.
      ENDIF.

    WHEN 'TMSSTAT'.
      PERFORM frm_get_tr USING ls_e070v-trkorr CHANGING lr_trs.
      SUBMIT /sdf/cmo_tr_check "VIA SELECTION-SCREEN
        WITH p_source = 'NONE'
        WITH p_target = cos_target
        WITH p_ori_tr IN lr_trs
        WITH p_crsref = abap_true
        WITH p_dgp = abap_true
        WITH p_swcomp = abap_true
        WITH p_imptim = abap_true
        AND RETURN.

    WHEN 'CODSTAT'.
      PERFORM frm_get_tr USING ls_e070v-trkorr CHANGING lr_trs.
      PERFORM frm_get_program USING  lr_trs
                            CHANGING lr_prog
                                     lr_fugr
                                     lr_func
                                     lr_clas
                                     lr_fugr1.
      DATA(lr_devcl) = VALUE /sdf/cd_cc_rng_package( ( sign = 'I' option = 'CP' low = cos_devcl ) ).
      DATA(lr_object) = VALUE typ_r_objecttype( ( sign = 'I' option = 'EQ' low = 'PROG' )
                                                ( sign = 'I' option = 'EQ' low = 'FUGR' )
                                                ( sign = 'I' option = 'EQ' low = 'CLAS' ) ).
      DATA(lr_name) = VALUE typ_r_object( ).
      APPEND LINES OF VALUE typ_r_object( FOR wa1 IN lr_prog ( sign = wa1-sign option = wa1-option low = wa1-low high = wa1-high ) ) TO lr_name.
      APPEND LINES OF VALUE typ_r_object( FOR wa2 IN lr_fugr1 ( sign = wa2-sign option = wa2-option low = wa2-low high = wa2-high ) ) TO lr_name.
      APPEND LINES OF VALUE typ_r_object( FOR wa3 IN lr_clas ( sign = wa3-sign option = wa3-option low = wa3-low high = wa3-high ) ) TO lr_name.
      DATA(lr_datum1) = VALUE trgr_date( ( sign = 'I' option = 'BT' low = cos_date_create high = sy-datum ) ).
      SUBMIT /sdf/cd_custom_code_metric "VIA SELECTION-SCREEN
        WITH s_devcl IN lr_devcl
        WITH s_object IN lr_object
        WITH s_name IN lr_name
        WITH s_trkorr IN lr_trs "无效条件，忽略
        WITH s_date IN lr_datum1  "无效条件，忽略
*        WITH c_vrsd = abap_true
*        WITH c_ddeep = abap_true
        WITH c_stmts = abap_true
*        WITH c_diff = abap_true
*        WITH c_mod = abap_true
*        WITH c_mccabe = abap_true
*        WITH c_transp = abap_true
        AND RETURN.

    WHEN 'OTHSTAT'. " Check明细
      DATA(lt_e071) = ls_e070v-detail.
      CHECK lt_e071 IS NOT INITIAL.
      TRY.
          LOOP AT lt_e071 ASSIGNING FIELD-SYMBOL(<fs_e071>).
            <fs_e071>-as4pos = sy-tabix.
          ENDLOOP.
          CALL METHOD cl_salv_table=>factory
            IMPORTING
              r_salv_table = DATA(lo_salv_table)
            CHANGING
              t_table      = lt_e071.
          lo_salv_table->get_display_settings( )->set_striped_pattern( abap_true ).
          lo_salv_table->get_display_settings( )->set_list_header( |Error Objects, Total: { lines( lt_e071 ) }| ).
          lo_salv_table->set_screen_popup(
            start_column = 10
            end_column   = 100
            start_line   = 1
            end_line     = 20
          ).
          DATA(lo_columns) = lo_salv_table->get_columns( ).
          lo_columns->set_optimize( abap_true ).
          lo_columns->set_key_fixation( abap_true ).
          DATA(lt_columns_ref) = lo_columns->get( ).
          LOOP AT lt_columns_ref ASSIGNING FIELD-SYMBOL(<fs_columns_ref>).
            CASE <fs_columns_ref>-columnname.
              WHEN 'OBJECT' OR 'OBJ_NAME' OR 'AS4POS' OR 'ACTIVITY'.
              WHEN OTHERS.
                <fs_columns_ref>-r_column->set_technical( abap_true ).
            ENDCASE.
          ENDLOOP.
          lo_salv_table->get_functions( )->set_all( abap_true ).
          lo_salv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
          lo_salv_table->display( ).
        CATCH cx_salv_msg .
          RETURN.
      ENDTRY.
      CLEAR lt_e071.

    WHEN OTHERS.
      CALL FUNCTION 'TR_PRESENT_REQUEST'
        EXPORTING
          iv_trkorr   = ls_e070v-trkorr
          iv_showonly = p_disp.

  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_single_click
*&---------------------------------------------------------------------*
FORM frm_single_click  USING    pv_row  TYPE  salv_de_row
                                pv_column TYPE  salv_de_column.

  READ TABLE gt_e070v INDEX pv_row ASSIGNING FIELD-SYMBOL(<fs_e070v>).

  CASE pv_column.
    WHEN 'CHECK'.
      IF <fs_e070v>-check IS INITIAL.
        <fs_e070v>-check = 'X'. "选择
      ELSE.
        <fs_e070v>-check = ' '. "取消选择
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

  go_salv_table->refresh( ). "刷新数据

ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_get_tr
*&---------------------------------------------------------------------*
FORM frm_get_tr  USING pv_tr CHANGING pr_trs TYPE cnvc_scwb_tr.

  DATA: lt_e070v_srt TYPE SORTED TABLE OF ty_e070v WITH NON-UNIQUE KEY check.
  lt_e070v_srt = gt_e070v.
  DATA(lt_sel_e070v) = FILTER #( lt_e070v_srt EXCEPT WHERE check = ' ' ).
  pr_trs = VALUE #( FOR wa IN lt_sel_e070v ( sign = 'I' option = 'EQ' low = wa-trkorr ) ).
  APPEND VALUE #( sign = 'I' option = 'EQ' low = pv_tr ) TO pr_trs.
  SORT pr_trs.
  DELETE ADJACENT DUPLICATES FROM pr_trs COMPARING ALL FIELDS.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_get_program
*&---------------------------------------------------------------------*
FORM frm_get_program  USING    pr_trs TYPE cnvc_scwb_tr
                      CHANGING pr_prog LIKE gr_prog
                               pr_fugr LIKE gr_fugr
                               pr_func LIKE gr_func
                               pr_clas LIKE gr_clas
                               pr_fugr1 LIKE gr_fugr.

  DATA(lr_object1) = VALUE devtyrange( ( sign = 'I' option = 'EQ' low = 'PROG' )
                                       ( sign = 'I' option = 'EQ' low = 'INCL' )
                                       ( sign = 'I' option = 'EQ' low = 'REPS' ) ).
  DATA(lr_object2) = VALUE devtyrange( ( sign = 'I' option = 'EQ' low = 'FUGR' ) ).
  DATA(lr_object3) = VALUE devtyrange( ( sign = 'I' option = 'EQ' low = 'FUNC' ) ).
  DATA(lr_object4) = VALUE devtyrange( ( sign = 'I' option = 'EQ' low = 'CLAS' )
                                       ( sign = 'I' option = 'EQ' low = 'METH' ) ).
  DATA(lr_object) = lr_object1.
  lr_object = CORRESPONDING #( BASE ( lr_object ) lr_object2 ).
  lr_object = CORRESPONDING #( BASE ( lr_object ) lr_object3 ).
  lr_object = CORRESPONDING #( BASE ( lr_object ) lr_object4 ).

*  gr_trs = VALUE cnvc_scwb_tr( FOR wa IN gt_e070v ( sign = 'I' option = 'EQ' low = wa-trkorr ) ).
  gr_trs = pr_trs.
  SELECT 'I' AS sign,
        'EQ' AS option,
        e~trkorr AS low
    FROM e070v AS e
    WHERE e~strkorr IN @gr_trs
    INTO TABLE @DATA(lt_childtr).
  APPEND LINES OF lt_childtr TO gr_trs.

  SELECT *
    FROM e071 AS e
    WHERE e~trkorr IN @gr_trs
      AND e~pgmid <> 'LANG'
      AND e~object IN @lr_object
    INTO TABLE @DATA(lt_e071_all).

  LOOP AT lt_e071_all ASSIGNING FIELD-SYMBOL(<fs_e071>) WHERE object IN lr_object4.
    SPLIT <fs_e071>-obj_name AT space INTO DATA(lv_name1) DATA(lv_name2).
    IF sy-subrc = 0.
      <fs_e071>-obj_name = lv_name1.
    ENDIF.
  ENDLOOP.

  DATA(lt_e071) = VALUE e071_t( FOR wa5 IN lt_e071_all WHERE ( trkorr IN gr_trs ) ( wa5 ) ).
  pr_prog = VALUE #( FOR wa1 IN lt_e071 WHERE ( object IN lr_object1 ) ( sign = 'I' option = 'EQ' low = wa1-obj_name ) ).
  pr_fugr = VALUE #( FOR wa2 IN lt_e071 WHERE ( object IN lr_object2 ) ( sign = 'I' option = 'EQ' low = wa2-obj_name ) ).
  pr_func = VALUE #( FOR wa3 IN lt_e071 WHERE ( object IN lr_object3 ) ( sign = 'I' option = 'EQ' low = wa3-obj_name ) ).
  pr_clas = VALUE #( FOR wa4 IN lt_e071 WHERE ( object IN lr_object4 ) ( sign = 'I' option = 'EQ' low = wa4-obj_name ) ).
  SORT pr_prog.
  DELETE ADJACENT DUPLICATES FROM pr_prog COMPARING ALL FIELDS.
  SORT pr_fugr.
  DELETE ADJACENT DUPLICATES FROM pr_fugr COMPARING ALL FIELDS.
  SORT pr_func.
  DELETE ADJACENT DUPLICATES FROM pr_func COMPARING ALL FIELDS.
  SORT pr_clas.
  DELETE ADJACENT DUPLICATES FROM pr_clas COMPARING ALL FIELDS.

  IF pr_func IS NOT INITIAL.
    SELECT 'I' AS sign,
          'EQ' AS option,
          replace( f~pname, 'SAPL', ' ' ) AS low
      FROM tfdir AS f
      WHERE f~funcname IN @pr_func
      INTO TABLE @DATA(lt_fugr1).
  ENDIF.
  IF lt_fugr1 IS NOT INITIAL.
    pr_fugr1 = VALUE #( FOR wa6 IN lt_fugr1 ( sign = wa6-sign option = wa6-option low = wa6-low ) ).
    pr_fugr1 = CORRESPONDING #( BASE ( pr_fugr1 ) pr_fugr ).
    SORT pr_fugr1.
    DELETE ADJACENT DUPLICATES FROM pr_fugr1 COMPARING ALL FIELDS.
  ELSE.
    pr_fugr1 = pr_fugr.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_check_trkorr
*&---------------------------------------------------------------------*
FORM frm_check_trkorr  USING    pv_obj_name
                                pv_trkorr
                                pv_string
                       CHANGING cv_cdlstat.

  DATA lt_source TYPE TABLE OF string.
  READ REPORT pv_obj_name INTO lt_source.
  IF sy-subrc <> 0 OR lines( lt_source ) LE 2.
    RETURN.
  ENDIF.

  FIND pv_trkorr IN TABLE lt_source MATCH LINE DATA(lv_idx).  "记录TR号
  IF sy-subrc <> 0.
    LOOP AT lt_source INTO DATA(lv_line).
      CONDENSE lv_line.
      CHECK lv_line IS NOT INITIAL.
      IF lv_line(1) = '*' OR lv_line(1) = '"'.
        DELETE lt_source INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
    IF lines( lt_source ) GT 2.
      cv_cdlstat = icon_red_light.
    ENDIF.
    RETURN.
  ENDIF.

  READ TABLE lt_source INTO DATA(ls_source) INDEX lv_idx.
  CHECK sy-subrc = 0.
  IF ls_source CS pv_string.  "记录变更号
    cv_cdlstat = icon_green_light.
  ELSE.
    cv_cdlstat = icon_red_light.
  ENDIF.

ENDFORM.
```