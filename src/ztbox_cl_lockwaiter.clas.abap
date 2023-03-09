class ZTBOX_CL_LOCKWAITER definition
  public
  final
  create public .

public section.

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR
    importing
      !I_TABLE type ROOTTAB .
  methods SET_OBJECT
    importing
      !IS_KEYS type ANY .
  methods WAIT_FOR
    importing
      !I_ENDLESSLY type FLAG optional .
  methods GET_MESSAGE
    returning
      value(R_MESS) type STRING .
  methods LOCK
    importing
      !I_ENQ_MODE type C default 'E' .
  methods UNLOCK .
protected section.
private section.

  types:
    BEGIN OF ty_key_fields,
      fieldname TYPE feld_name,
      value     TYPE string,
    END OF ty_key_fields .
  types:
    ty_key_fields_t TYPE TABLE OF ty_key_fields WITH KEY fieldname .
  types:
    BEGIN OF ty_lock_modes,
           lock_mode TYPE eqegramode,
           collision TYPE eqegramode,
         END OF ty_lock_modes .
  types:
    ty_lock_modes_t TYPE TABLE OF ty_lock_modes WITH DEFAULT KEY .

  data _ROOT_TAB type ROOTTAB .
  data _DEQUEUE_CALLER type ref to ZTBOX_CL_FMODULER .
  data _ENQUEUE_CALLER type ref to ZTBOX_CL_FMODULER .
  data _MESSAGE type STRING .
  data _ENQ_MODE type C .
  data _KEY_FIELDS type TY_KEY_FIELDS_T .
  constants C_FOREIGN_LOCK type CHAR30 value 'FOREIGN_LOCK' ##NO_TEXT.
  class-data _LOCK_MODES type TY_LOCK_MODES_T .
  data _COLLISION_LOCK type EQEGRAMODE .

  methods _SET_FUNCTIONS .
  methods _GET_FIELDS_LIST
    returning
      value(R_FIELDS) type DDFIELDS .
  methods _SET_KEY_FIELDS .
  methods _MAP_KEYS
    importing
      !IS_KEYS type ANY .
  class-methods _SET_LOCK_MODES .
  methods _SET_COLLISION_LOCK .
ENDCLASS.



CLASS ZTBOX_CL_LOCKWAITER IMPLEMENTATION.


  METHOD constructor.

    _root_tab = i_table.

    _set_functions( ).

    _set_key_fields( ).

  ENDMETHOD.


  METHOD lock.

    _enqueue_caller->free( ).

    _enq_mode = i_enq_mode.

    LOOP AT _key_fields INTO DATA(key).

      _enqueue_caller->exporting( i_name = key-fieldname i_value = key-value ).

    ENDLOOP.

    _enqueue_caller->exporting( i_name = |MODE_{ _root_tab }| i_value = i_enq_mode ).

    _enqueue_caller->execute( ).

    _message = _enqueue_caller->exception( )-message.

  ENDMETHOD.


  METHOD unlock.

    _dequeue_caller->free( ).

    LOOP AT _key_fields INTO DATA(key).

      _dequeue_caller->exporting( i_name = key-fieldname i_value = key-value ).

    ENDLOOP.

    _dequeue_caller->exporting( i_name = |MODE_{ _root_tab }| i_value = _enq_mode ).

    _dequeue_caller->execute( ).

    _message = _enqueue_caller->exception( )-message.

  ENDMETHOD.


  METHOD wait_for.

    CHECK _collision_lock IS NOT INITIAL.
    CLEAR _message.

    _enqueue_caller->free( ).

    LOOP AT _key_fields INTO DATA(key).

      _enqueue_caller->exporting( i_name = key-fieldname i_value = key-value ).

    ENDLOOP.

    _enqueue_caller->exporting( i_name = |MODE_{ _root_tab }| i_value = _collision_lock ).
    _enqueue_caller->exporting( i_name = |_WAIT| i_value = abap_true ).

    IF i_endlessly EQ abap_true.

      DO.
        _enqueue_caller->execute( ).
        IF _enqueue_caller->exception( )-except NE c_foreign_lock.
          EXIT.
        ENDIF.
      ENDDO.

    ELSE.

      _enqueue_caller->execute( ).
      _message = _enqueue_caller->exception( )-message.

    ENDIF.

  ENDMETHOD.


  METHOD set_object.

    _map_keys( is_keys ).

    _set_collision_lock( ).

  ENDMETHOD.


  METHOD _get_fields_list.

    CLEAR r_fields.

    DATA(table_descr) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( _root_tab ) ).
    CHECK table_descr IS BOUND.

    table_descr->get_ddic_field_list(
      RECEIVING
        p_field_list = r_fields
      EXCEPTIONS
        no_ddic_type  = 1
        not_found     = 2 ).

    SORT r_fields BY position.

    READ TABLE r_fields ASSIGNING FIELD-SYMBOL(<mandt>) WITH KEY domname = 'MANDT'.
    IF sy-subrc EQ 0.
      <mandt>-fieldname = 'MANDT'.
    ENDIF.

  ENDMETHOD.


  METHOD _map_keys.

    LOOP AT _key_fields ASSIGNING FIELD-SYMBOL(<key>).

      CLEAR <key>-value.

      IF <key>-fieldname EQ 'MANDT'.
        <key>-value = sy-mandt.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT <key>-fieldname OF STRUCTURE is_keys TO FIELD-SYMBOL(<key_value>).
      CHECK sy-subrc EQ 0.

      <key>-value = <key_value>.

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_functions.

    SELECT SINGLE viewname
      FROM dd25l
      WHERE roottab   EQ @_root_tab
        AND as4local  EQ 'A'
        AND as4vers   EQ '0000'
        AND aggtype   EQ 'E'
       INTO @DATA(enqueue).
    CHECK sy-subrc EQ 0.

    _enqueue_caller = NEW ztbox_cl_fmoduler( |ENQUEUE_{ enqueue }| ).
    _dequeue_caller = NEW ztbox_cl_fmoduler( |DEQUEUE_{ enqueue }| ).

  ENDMETHOD.


  METHOD _set_key_fields.

    DATA(fields_list) = _get_fields_list( ).

    _key_fields = VALUE #( FOR field IN fields_list WHERE ( keyflag EQ abap_true )
      ( fieldname = field-fieldname ) ).

  ENDMETHOD.


  METHOD get_message.

    r_mess = _message.

  ENDMETHOD.


  METHOD class_constructor.

    _set_lock_modes( ).

  ENDMETHOD.


  METHOD _set_collision_lock.

    DATA enq_tab  TYPE TABLE OF seqg3.
    DATA gname    TYPE eqegraname.
    DATA garg     TYPE eqegraarg.

    gname = _root_tab.

    LOOP AT _key_fields INTO DATA(key).
      garg = COND #( WHEN sy-tabix EQ 1 THEN |{ key-value }| ELSE |{ garg }{ key-value }| ).
    ENDLOOP.

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        gclient               = sy-mandt
        gname                 = gname
        garg                  = garg
      TABLES
        enq                   = enq_tab
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.

    CHECK sy-subrc EQ 0.

    DATA(gmode) = VALUE #( enq_tab[ 1 ]-gmode OPTIONAL ).

    _collision_lock = VALUE #( _lock_modes[ collision = gmode ]-lock_mode OPTIONAL ).

  ENDMETHOD.


  METHOD _set_lock_modes.

    _lock_modes = VALUE #(
      ( lock_mode = 'U' collision = 'X' )
      ( lock_mode = 'V' collision = 'E' )
      ( lock_mode = 'W' collision = 'S' ) ).

  ENDMETHOD.
ENDCLASS.
