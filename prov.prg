SET PROCEDURE TO PROV
SET DELETED ON
SET POINT TO ","
SET SEPARATOR TO "."
SET TALK OFF
SET CENTURY ON
SET DATE ITALIAN
_screen.FontSize=15
_screen.FontName="courier new"
_screen.Caption="Programa Proveedores"
A="USE C:\vispagos\PROVEEDO INDEX C:\vispagos\INPROV,C:\vispagos\IN1CLAVE,c:\vispagos\innif"

B="USE C:\vispagos\PROVEEDO INDEX C:\vispagos\IN1CLAVE,C:\vispagos\INPROV,c:\vispagos\innif"
b1="use c:\vispagos\proveedo index c:\vispagos\innif,c:\vispagos\in1clave,c:\vispagos\innombre"

C="USE C:\visPROV\FACPROV INDEX C:\visPROV\INPROVNU"

DO WHILE .T.

  RESP="e"
  CLEAR
  *@  5,5 SAY "(M)ANTENIMIENTO DE PROVEEDORES"
  *@  7,5 SAY "(E)NTRADA DE FACTURAS"
       ****OPCION BORRAR MODIFICAR BAJAS EN CASO DE QUE EL NUMERO YA
       ****EXISTA PARA EL CLIENTE

  *@  9,5 SAY "(L)ISTADO POR PROVEEDOR ANUAL/MENSUAL"
  *@  11,5 SAY "LISTADO ME(N)SUAL DE PROVEEDORES PAGADO/NO"
       ****ORDENADO POR PROVEEDOR, CON TOTAL
  *@  13,5 SAY "LISTADO (A)GRUPADO ANUAL/MENSUAL"
       ****CON TOTAL
  *@ 15,5 SAY "(S)INCRONIZACION CON PAGOS"
       ****DEL FICHERO PROVEEDORES EN AMBOS
  *@ 17,5 SAY "REINDE(X)AR"
  *@ 19,5 SAY "(Q)UIT"
  *@ 21,5 say "SALI(R) "

  *@ 23,5 SAY "                                          " GET RESP
  **** EL FICHERO PROVEEDORES CON LA MISMA ESTRUCTURA QUE EL DE PAGOS

  **** DOS FICHEROS: FACprov Y PROVEEDORES
  **** FICHERO FACprov   indice inprovnum
  ****      CAMPOS    PROVEEDOR(CLAVE)  c 10
     *                N§ FACTURA        c 10
     *                FECHA
     *                IMPORTE           n 10
     *                PAGADO/NO
     *                REFERENCIA AGENCIA (4 POSIBLES) c 10
  **** FICHERO PROVEEDORES  indices inprov, inclave, innif
  ****      CAMPOS
     *                NOMBRE            c 40
     *                DIRECCION         c 40
     *                CODPOSTAL         c  8
     *                POBLACION         c 30
     *                PROVINCIA         c 10
     *                PAIS              c 10
     *                NIF               c 16
     *                CLAVE             c 10
     *                DEPARTAMEN        c 40
     *                POLIGONO          c 40
     *                TELEFONO          c 16
     *                APDO              c 10
     *                CONTACTO          c 10
     *                ACTIVIDAD         c 12
     *                FORMAPAGO         c 50
     *                IMP               l  1
     *                BANCO             c 15
     *                N.CUENTA          c 30
  *READ
  DO FORM visprov_menuinicio
  
  RESP=LOWER(RESP)


DO CASE
      CASE RESP="q"
              QUIT
      CASE RESP="x"
              &A
              REINDEX
              &C
              REINDEX


      CASE RESP="m"

               DO MANCLI
      case resp="e"
               do entrada

      case resp="l"

               do listado
      case resp="n"
               do lismen

      case resp="a"
               do lisagr

      case resp="s"
               do sincro

      case resp="r"
               CLOSE INDEX
               CLOSE DATABASES
               SET TALK ON
               exit

ENDCASE


ENDDO
****************************************************************
****************************************************************
****************************************************************
******** P R O C E D I M I E N T O S
****************************************************************
****************************************************************
****************************************************************


*******************************************
********MANTENIMIENTO DE PROVEEDORES
************************************

PROCEDURE MANCLI
volver=" "
SEGUIR=.T.
do while SEGUIR
  clear
  @ 1,1 to 26,90 double
  @ 4,9 to 7,50
  @ 5,10 SAY "MANTENIMIENTO PROVEEDORES"

  @ 10,10 say "(A)ltas"
  @ 12,10 say "(B)ajas"
  @ 14,10 say "(M)odificaciones"
  @ 16,10 say "(C)onsultas"
  @ 18,10 say "Consulta por c(l)ave"
  @ 20,10 say "Consulta por (n)if"
  @ 22,10 say "(V)olver        " get volver
  read
  volver=lower(volver)
  do case
       case volver="a"


            &A

            store space(40) to wnombre
            store space(40) to wdireccion
            store space(6)  to wcodpostal
            store space(30) to wpoblacion
            store space(10) to wprovincia
            store space(10) to wpais
            store space(16) to wnif
            store space(10) to wclave
            store space(40) to wdepartamen
            store space(40) to wpoligono
            store space(16) to wtelefono
            store space(10) to wapdo
            store space(10) to wcontacto
            store space(12) to wactividad
            store space(50) to wformapago
            store space(15) to wbanco
            store space(30) to wn_cuenta

            clear
            @ 6,5 say  "Nombre:        " get wnombre
            @ 7,5 say  "Direccion:     " get wdireccion
            @ 8,5 say  "Codigo Postal: " get wcodpostal
            @ 9,5 say  "Poblacion:     " get wpoblacion
            @ 10,5 say "Provincia:     " get wprovincia
            @ 11,5 say "Pais:          " get wpais
            @ 12,5 say "Nif:           " get wnif
            @ 13,5 say "Clave:         " get wclave
            @ 14,5 say "Departamento:  " get wdepartamen
            @ 15,5 say "Poligono:      " get wpoligono
            @ 16,5 say "Telefono:      " get wtelefono
            @ 17,5 say "Apartado:      " get wapdo
            @ 18,5 say "Contacto:      " get wcontacto
            @ 19,5 say "Actividad:     " get wactividad
            @ 20,5 say "Forma de pago: " get wformapago
            @ 21,5 say "Banco:         " get wbanco
            @ 22,5 say "Nº cuenta:     " get wn_cuenta


            read
            wnformapago=wformapago
            IF WCLAVE=" "
              CLEAR

               @ 13,5 say "Es necesaria la clave: " get wclave
              READ
            ENDIF
            append blank
            replace nombre with wnombre
            replace direccion with wdireccion
            replace codpostal with wcodpostal
            replace poblacion with wpoblacion
            replace provincia with wprovincia
            replace pais with wpais
            replace nif with wnif
            replace clave with wclave
            replace departamen with wdepartamen
            replace poligono with wpoligono
            replace telefono with wtelefono
            replace apdo with wapdo
            replace contacto with wcontacto
            replace actividad with wactividad
            replace formapago with wformapago
            replace banco     with wbanco
            replace n_cuenta  with wn_cuenta


       case volver="b"

              &A
            clear
            store space(40) to wnombre
            @ 10,5 say "Nombre: " get wnombre
            read
            wnombre=upper(wnombre)
            wnombre=trim(wnombre)
            SEEK wnombre
            browse fields nombre,poblacion noappend nomenu NOEDIT NODELETE
            clear
            @ 10,5 say " " +nombre
            wrespuesta="n"
            @ 11,5 say "¿Esta seguro de que desea borrarlo? s/n" get wrespuesta
            read
            wrespuesta=lower(wrespuesta)
            if wrespuesta="s"
                    delete
                    wait WINDOW "El cliente ha sido borrado"
            endif
       case volver="m"



              &A
            clear
            store space(40) to wnombre
            @ 10,5 say "Nombre: " get wnombre
            read
            wnombre=upper(wnombre)
            wnombre=trim(wnombre)
            SEEK wnombre
            browse fields nombre,poblacion noappend nomenu NOEDIT NODELETE
            clear
            @ 6,5 say  "Nombre:        " get nombre
            @ 7,5 say  "Direccion:     " get direccion
            @ 8,5 say  "Codigo Postal: " get codpostal
            @ 9,5 say  "Poblacion:     " get poblacion
            @ 10,5 say "Provincia:     " get provincia
            @ 11,5 say "Pais:          " get pais
            @ 12,5 say "Nif:           " get nif
            @ 13,5 say "Clave:         " +clave
            @ 14,5 say "Departamento:  " get departamen
            @ 15,5 say "Poligono:      " get poligono
            @ 16,5 say "Telefono:      " get telefono
            @ 17,5 say "Apartado:      " get apdo
            @ 18,5 say "Contacto:      " get contacto
            @ 19,5 say "Actividad:     " get actividad
            @ 20,5 say "Forma de pago: " get formapago

            @ 21,5 say "Banco:         " get banco
            @ 22,5 say "Nº cuenta:     " get n_cuenta

            read

       case volver="c"


             &A
            store space(40) to wnombre
            clear
            @ 6,15 say "Introduce el cliente: " get wnombre
            read
            wprueba=trim(wnombre)
            wprueba=upper(wprueba)
            SEEK wprueba
            browse fields nombre,POBLACION NOMENU NOAPPEND NOEDIT NODELETE
            clear

     @ 3,10 say  "Nombre:        " +nombre
     @ 4,10 say  "Direccion:     " +direccion
     @ 5,10 say  "Codigo Postal: " +codpostal
     @ 6,10 say  "Poblacion:     " +poblacion
     @ 7,10 say  "Provincia:     " +provincia
     @ 8,10 say  "Pais:          " +pais
     @ 9,10 say  "Nif:           " +nif
     @ 10,10 say "Clave:         " +clave
     @ 11,10 say "Departamento:  " +departamen
     @ 12,10 say "Poligono:      " +poligono
     @ 13,10 say "Telefono:      " +telefono
     @ 14,10 say "Apartado:      " +apdo
     @ 15,10 say "Contacto:      " +contacto
     @ 16,10 say "Actividad:     " +actividad
     @ 17,10 say "Forma de pago: " +formapago

     @ 18,5 say "Banco:         " +banco
     @ 19,5 say "Nº cuenta:     " +n_cuenta
            ?
            ?
            WAIT window


       case volver="l"

            &B
            store space(10) to wclave
            clear
            @ 6,15 say "Introduce la clave del cliente: " get wclave
            read
            wprueba=trim(wclave)
            wprueba=upper(wprueba)
            SEEK wprueba
            browse fields clave,POBLACION NOMENU NOAPPEND NOEDIT NODELETE
            clear

     @ 3,10 say  "Nombre:        " +nombre
     @ 4,10 say  "Direccion:     " +direccion
     @ 5,10 say  "Codigo Postal: " +codpostal
     @ 6,10 say  "Poblacion:     " +poblacion
     @ 7,10 say  "Provincia:     " +provincia
     @ 8,10 say  "Pais:          " +pais
     @ 9,10 say  "Nif:           " +nif
     @ 10,10 say "Clave:         " +clave
     @ 11,10 say "Departamento:  " +departamen
     @ 12,10 say "Poligono:      " +poligono
     @ 13,10 say "Telefono:      " +telefono
     @ 14,10 say "Apartado:      " +apdo
     @ 15,10 say "Contacto:      " +contacto
     @ 16,10 say "Actividad:     " +actividad
     @ 17,10 say "Forma de pago: " +formapago

     @ 18,5 say "Banco:         " +banco
     @ 19,5 say "Nº cuenta:     " +n_cuenta
            ?
            ?
            WAIT window
        case volver="n"
          &b1
                      store space(10) to wnif
            clear
            @ 6,15 say "Introduce el nif del cliente: " get wnif
            read
            wprueba=trim(wnif)
            wprueba=upper(wprueba)
            SEEK wprueba
            browse fields nif,nombre,POBLACION NOMENU NOAPPEND NOEDIT NODELETE
            clear

     @ 3,10 say  "Nombre:        " +nombre
     @ 4,10 say  "Direccion:     " +direccion
     @ 5,10 say  "Codigo Postal: " +codpostal
     @ 6,10 say  "Poblacion:     " +poblacion
     @ 7,10 say  "Provincia:     " +provincia
     @ 8,10 say  "Pais:          " +pais
     @ 9,10 say  "Nif:           " +nif
     @ 10,10 say "Clave:         " +clave
     @ 11,10 say "Departamento:  " +departamen
     @ 12,10 say "Poligono:      " +poligono
     @ 13,10 say "Telefono:      " +telefono
     @ 14,10 say "Apartado:      " +apdo
     @ 15,10 say "Contacto:      " +contacto
     @ 16,10 say "Actividad:     " +actividad
     @ 17,10 say "Forma de pago: " +formapago

     @ 18,5 say "Banco:         " +banco
     @ 19,5 say "Nº cuenta:     " +n_cuenta
            ?
            ?
            WAIT window

        case volver="v"

                 
                 SEGUIR=.F.
     endcase
enddo











RETURN


*****************************************
*********ENTRADA DE FACTURAS
*****************************************


PROCEDURE ENTRADA

  **** FICHERO FACprov
  ****      CAMPOS    PROVEEDOR(CLAVE)  c 10
     *                N§ FACTURA        c 10
     *                FECHA
     *                IMPORTE           n 10
     *                PAGADO/NO
     *                REFERENCIA AGENCIA (4 POSIBLES) c 10


CLEAR


DO WHILE .T.

    CON = "v"
    CLEAR
    @ 4,5 say "VARIAS ALTAS DE UN SOLO (P)ROVEEDOR"
    @ 5,5 SAY "(A)LTA DE FACTURA"
    @ 6,5 SAY "(M)ODIFICACION"
    @ 7,5 SAY "(B)AJAS"
    @ 8,5 SAY "(V)OLVER"
    @ 9,5 SAY "               " GET CON

    READ
    CON = LOWER(CON)

    DO CASE
         CASE CON = "p"

                    ************************************
                    ******* ALTAS DE ENTRADA DE VARIAS FACTURAS
                    ************************************

                    WPROV = SPACE(40)





                    CLEAR

                    @ 5,5 SAY "INTRODUCE PROVEEDOR" GET WPROV
                    READ
                    WPROV = UPPER(WPROV)
                    wprov=trim(wprov)

              &A
                    SEEK WPROV
                    BROW FIEL NOMBRE,POBLACION NOMENU NOAPPEND NOEDIT NODELETE
                    CLEAR

                    RESP = "s"
                    @ 2,2 SAY "ENCONTRADO S/N" GET RESP
                    READ
                    RESP = LOWER(RESP)

                    IF RESP="s"
                               WCLAVE  = CLAVE
                               WNOMBRE = NOMBRE
                    ELSE


           &B



           STORE SPACE(40) TO WNOMBRE
           STORE SPACE(40) TO WDIRECCION
           STORE SPACE(6)  TO WCODPOSTAL
           STORE SPACE(30) TO WPOBLACION
           STORE SPACE(10) TO WPROVINCIA
           STORE SPACE(10) TO WPAIS
           STORE SPACE(16) TO WNIF
           STORE SPACE(10) TO WCLAVE
           STORE SPACE(40) TO WDEPARTAMEN
           STORE SPACE(40) TO WPOLIGONO
           STORE SPACE(16) TO WTELEFONO
           STORE SPACE(10) TO WAPDO
           STORE SPACE(10) TO WCONTACTO
           STORE SPACE(12) TO WACTIVIDAD
           STORE SPACE(50) TO WFORMAPAGO
           STORE SPACE(15) TO WBANCO
           STORE SPACE(30) TO WN_CUENTA



           CLEAR


           @ 6,5  SAY "NOMBRE:         " GET WNOMBRE
           @ 7,5  SAY "DIRECCION:      " GET WDIRECCION
           @ 8,5  SAY "CODIGO POSTAL:  " GET WCODPOSTAL
           @ 9,5  SAY "POBLACION:      " GET WPOBLACION
           @ 10,5 SAY "PROVINCIA:      " GET WPROVINCIA
           @ 11,5 SAY "PAIS:           " GET WPAIS
           @ 12,5 SAY "NIF:            " GET WNIF
           @ 13,5 SAY "CLAVE:          " GET WCLAVE
           @ 14,5 SAY "DEPARTAMENTO    " GET WDEPARTAMEN
           @ 15,5 SAY "POLIGONO:       " GET WPOLIGONO
           @ 16,5 SAY "TELEFONO:       " GET WTELEFONO
           @ 17,5 SAY "APARTADO:       " GET WAPDO
           @ 18,5 SAY "CONTACTO:       " GET WCONTACTO
           @ 19,5 SAY "ACTIVIDAD:      " GET WACTIVIDAD
           @ 20,5 SAY "FORMA DE PAGO:  " GET WFORMAPAGO
           @ 21,5 SAY "BANCO:          " GET WBANCO
           @ 22,5 SAY "Nº CUENTA:      " GET WN_CUENTA


           READ


           IF WCLAVE=" "
             CLEAR

             @ 13,5 SAY "ES NECESARIA LA CLAVE: " GET WCLAVE
             READ
           ENDIF


           SEEK WCLAVE
           IF FOUND()
                 CLEAR
                 @ 13,5 SAY "LA CLAVE "+WCLAVE+" YA EXISTE"
                 WAIT window
                 RETURN
                * @ 14,5 SAY "ELIJA OTRA: " GET WCLAVE
                * READ
           ENDIF

           APPEND BLANK
           REPLACE NOMBRE WITH WNOMBRE
           REPLACE DIRECCION WITH WDIRECCION
           REPLACE CODPOSTAL WITH WCODPOSTAL
           REPLACE POBLACION WITH WPOBLACION
           REPLACE PROVINCIA WITH WPROVINCIA
           REPLACE PAIS WITH WPAIS
           REPLACE NIF WITH WNIF
           REPLACE CLAVE WITH WCLAVE
           REPLACE DEPARTAMEN WITH WDEPARTAMEN
           REPLACE POLIGONO WITH WPOLIGONO
           REPLACE TELEFONO WITH WTELEFONO
           REPLACE APDO WITH WAPDO
           REPLACE CONTACTO WITH WCONTACTO
           REPLACE ACTIVIDAD WITH WACTIVIDAD
           REPLACE FORMAPAGO WITH WFORMAPAGO
           REPLACE BANCO WITH WBANCO
           REPLACE N_CUENTA WITH WN_CUENTA
        ENDIF
        USE
        STORE SPACE(10) TO WNUMFAC

        RESP="s"

     DO WHILE RESP="s"

        CLEAR


        @ 5,5 SAY "INTRODUCE Nº DE FACTURA" GET WNUMFAC

        READ

             &C
        WBUSQUEDA=WCLAVE+WNUMFAC
        SEEK WBUSQUEDA
        IF FOUND()
             CLEAR
             @ 5,5 SAY "LA FACTURA YA EXISTE"



             WAIT window

        ELSE
             WFECHA=CTOD("01-01-2004")
             WIMPORTE=0
             WPAGADO=.N.
             STORE SPACE(10) TO WREFAG1
             STORE SPACE(10) TO WREFAG2
             STORE SPACE(10) TO WREFAG3
             STORE SPACE(10) TO WREFAG4
             store space(10) to wrefag5
             store space(10) to wrefag6

             CLEAR
             @ 3,5  say "Proveedor: " +WNOMBRE
             @ 4,5  say "NUM. FACT. " +WNUMFAC

             @ 5,5  SAY "FECHA:     " GET WFECHA
             @ 6,5  SAY "IMPORTE:   " GET WIMPORTE  picture "99,999.99"
             @ 7,5  SAY "PAGADO/NO: " GET WPAGADO
             @ 8,5  SAY "REF. AG. 1 " GET WREFAG1
             @ 9,5  SAY "REF. AG. 2 " GET WREFAG2
             @ 10,5 SAY "REF. AG. 3 " GET WREFAG3
             @ 11,5 SAY "REF. AG. 4 " GET WREFAG4
             @ 12,5 say "REF. AG. 5 " GET WREFAG5
             @ 13,5 SAY "REF. AG. 6 " GET WREFAG6


             READ
         IF WIMPORTE<>0
             APPEND BLANK
             REPLACE CLAVE WITH WCLAVE
             REPLACE NUMFAC WITH WNUMFAC
             REPLACE FECHA WITH WFECHA
             REPLACE IMPORTE WITH WIMPORTE
             REPLACE PAGADO WITH WPAGADO
             REPLACE REFAG1 WITH WREFAG1
             REPLACE REFAG2 WITH WREFAG2
             REPLACE REFAG3 WITH WREFAG3
             REPLACE REFAG4 WITH WREFAG4

             REPLACE REFAG5 WITH WREFAG5
             REPLACE REFAG6 WITH WREFAG6
         ELSE
             CLEAR
             @ 5,5 SAY "NO SE HA GRABADO EL ULTIMO REGISTRO"
             WAIT window
         ENDIF


          ENDIF

          @ 16,5 SAY "¿MAS FACTURAS? " GET RESP
          READ

     ENDDO


         CASE CON = "a"

                VALOR=.F.
                IF VALOR
                    ************************************
                    ******* ALTAS DE ENTRADA DE FACTURAS
                    ************************************

                    WPROV = SPACE(40)





                    CLEAR

                    @ 5,5 SAY "INTRODUCE PROVEEDOR" GET WPROV
                    READ
                    WPROV = UPPER(WPROV)
                    wprov=trim(wprov)

              &A
                    SEEK WPROV
                    BROW FIEL NOMBRE,POBLACION NOMENU NOAPPEND NOEDIT NODELETE
                    CLEAR

                    RESP = "s"
                    @ 2,2 SAY "ENCONTRADO S/N" GET RESP
                    READ
                    RESP = LOWER(RESP)

                    IF RESP="s"
                               WCLAVE  = CLAVE
                               WNOMBRE = NOMBRE
                    ELSE


           &B



           STORE SPACE(40) TO WNOMBRE
           STORE SPACE(40) TO WDIRECCION
           STORE SPACE(6)  TO WCODPOSTAL
           STORE SPACE(30) TO WPOBLACION
           STORE SPACE(10) TO WPROVINCIA
           STORE SPACE(10) TO WPAIS
           STORE SPACE(16) TO WNIF
           STORE SPACE(10) TO WCLAVE
           STORE SPACE(40) TO WDEPARTAMEN
           STORE SPACE(40) TO WPOLIGONO
           STORE SPACE(16) TO WTELEFONO
           STORE SPACE(10) TO WAPDO
           STORE SPACE(10) TO WCONTACTO
           STORE SPACE(12) TO WACTIVIDAD
           STORE SPACE(50) TO WFORMAPAGO
           STORE SPACE(15) TO WBANCO
           STORE SPACE(30) TO WN_CUENTA



           CLEAR


           @ 6,5  SAY "NOMBRE:         " GET WNOMBRE
           @ 7,5  SAY "DIRECCION:      " GET WDIRECCION
           @ 8,5  SAY "CODIGO POSTAL:  " GET WCODPOSTAL
           @ 9,5  SAY "POBLACION:      " GET WPOBLACION
           @ 10,5 SAY "PROVINCIA:      " GET WPROVINCIA
           @ 11,5 SAY "PAIS:           " GET WPAIS
           @ 12,5 SAY "NIF:            " GET WNIF
           @ 13,5 SAY "CLAVE:          " GET WCLAVE
           @ 14,5 SAY "DEPARTAMENTO    " GET WDEPARTAMEN
           @ 15,5 SAY "POLIGONO:       " GET WPOLIGONO
           @ 16,5 SAY "TELEFONO:       " GET WTELEFONO
           @ 17,5 SAY "APARTADO:       " GET WAPDO
           @ 18,5 SAY "CONTACTO:       " GET WCONTACTO
           @ 19,5 SAY "ACTIVIDAD:      " GET WACTIVIDAD
           @ 20,5 SAY "FORMA DE PAGO:  " GET WFORMAPAGO
           @ 21,5 SAY "BANCO:          " GET WBANCO
           @ 22,5 SAY "Nº CUENTA:      " GET WN_CUENTA


           READ


           IF WCLAVE=" "
             CLEAR

             @ 13,5 SAY "ES NECESARIA LA CLAVE: " GET WCLAVE
             READ
           ENDIF


           SEEK WCLAVE
           IF FOUND()
                 CLEAR
                 @ 13,5 SAY "LA CLAVE "+WCLAVE+" YA EXISTE"
                 WAIT window
                 RETURN
                * @ 14,5 SAY "ELIJA OTRA: " GET WCLAVE
                * READ
           ENDIF

           APPEND BLANK
           REPLACE NOMBRE WITH WNOMBRE
           REPLACE DIRECCION WITH WDIRECCION
           REPLACE CODPOSTAL WITH WCODPOSTAL
           REPLACE POBLACION WITH WPOBLACION
           REPLACE PROVINCIA WITH WPROVINCIA
           REPLACE PAIS WITH WPAIS
           REPLACE NIF WITH WNIF
           REPLACE CLAVE WITH WCLAVE
           REPLACE DEPARTAMEN WITH WDEPARTAMEN
           REPLACE POLIGONO WITH WPOLIGONO
           REPLACE TELEFONO WITH WTELEFONO
           REPLACE APDO WITH WAPDO
           REPLACE CONTACTO WITH WCONTACTO
           REPLACE ACTIVIDAD WITH WACTIVIDAD
           REPLACE FORMAPAGO WITH WFORMAPAGO
           REPLACE BANCO WITH WBANCO
           REPLACE N_CUENTA WITH WN_CUENTA
        ENDIF
        USE
        STORE SPACE(10) TO WNUMFAC




        CLEAR


        @ 5,5 SAY "INTRODUCE Nº DE FACTURA" GET WNUMFAC

        READ

             &C
        WBUSQUEDA=WCLAVE+WNUMFAC
        SEEK WBUSQUEDA
        IF FOUND()
             CLEAR
             @ 5,5 SAY "LA FACTURA YA EXISTE"



             WAIT window

        ELSE
             WFECHA=CTOD("01-01-2003")
             WIMPORTE=0
             WPAGADO=.N.
             STORE SPACE(10) TO WREFAG1
             STORE SPACE(10) TO WREFAG2
             STORE SPACE(10) TO WREFAG3
             STORE SPACE(10) TO WREFAG4


             CLEAR
             @ 3,5  say "Proveedor: " +WNOMBRE
             @ 4,5  say "NUM. FACT. " +WNUMFAC

             @ 5,5  SAY "FECHA:     " GET WFECHA
             @ 6,5  SAY "IMPORTE:   " GET WIMPORTE  picture "99,999.99"
             @ 7,5  SAY "PAGADO/NO: " GET WPAGADO
             @ 8,5  SAY "REF. AG. 1 " GET WREFAG1
             @ 9,5  SAY "REF. AG. 2 " GET WREFAG2
             @ 10,5 SAY "REF. AG. 3 " GET WREFAG3
             @ 11,5 SAY "REF. AG. 4 " GET WREFAG4

             READ

             APPEND BLANK
             REPLACE CLAVE WITH WCLAVE
             REPLACE NUMFAC WITH WNUMFAC
             REPLACE FECHA WITH WFECHA
             REPLACE IMPORTE WITH WIMPORTE
             REPLACE PAGADO WITH WPAGADO
             REPLACE REFAG1 WITH WREFAG1
             REPLACE REFAG2 WITH WREFAG2
             REPLACE REFAG3 WITH WREFAG3
             REPLACE REFAG4 WITH WREFAG4

          ENDIF
         ENDIF  && DE VALOR
       CASE CON="m"
                ****************************************
                ****** MODIFICACIONES
                ****************************************

                    WPROV = SPACE(40)





                    CLEAR

                    @ 1,1 SAY "MODIFICACIONES DE ENTRADAS"

                    @ 5,5 SAY "INTRODUCE CLIENTE" GET WPROV
                    READ
                    Wprov = UPPER(Wprov)
                    wprov=trim(wprov)

              &A
                    SEEK WPROV
                    BROW FIEL NOMBRE,POBLACION NOMENU NOAPPEND NOEDIT NODELETE
                    CLEAR

                    RESP = "s"
                    @ 2,2 SAY "ENCONTRADO S/N" GET RESP
                    READ
                    RESP = LOWER(RESP)

      IF RESP="s"
                               WCLAVE  = CLAVE
                               WNOMBRE = NOMBRE


        USE
        STORE SPACE(10) TO WNUMFAC




        CLEAR


        @ 5,5 SAY "INTRODUCE Nº DE FACTURA" GET WNUMFAC

        READ
        *WNUMFAC=TRIM(WNUMFAC)
        *WCLAVE=TRIM(WCLAVE)

             &C
        WBUSQUEDA=WCLAVE+WNUMFAC
        SEEK WBUSQUEDA
        IF FOUND()
             CLEAR









             CLEAR

             @ 3,5  say "Proveedor: " +WNOMBRE
             @ 4,5  say "NUM. FACT. " +WNUMFAC
             @ 5,5  SAY "FECHA:     " GET FECHA
             @ 6,5  SAY "IMPORTE:   " GET IMPORTE PICTURE "99,999.99"
             @ 7,5  SAY "PAGADO/NO: " GET PAGADO
             @ 8,5  SAY "REF. AG. 1 " GET REFAG1
             @ 9,5  SAY "REF. AG. 2 " GET REFAG2
             @ 10,5 SAY "REF. AG. 3 " GET REFAG3
             @ 11,5 SAY "REF. AG. 4 " GET REFAG4

             @ 12,5 SAY "REF. AG. 5 " GET REFAG5
             @ 13,5 SAY "REF. AG. 6 " GET REFAG6
             READ
          else
           clear
           @ 5,5 say "NO SE ENCUENTRA LA FACTURA"
           WAIT window

          ENDIF
          use
          CASE CON="b"
                 ***********************************
                 ****BAJAS DE ENTRADA DE FACTURAS
                 ***********************************

                    WPROV = SPACE(40)





                    CLEAR

                    @ 1,1 SAY "BAJAS DE ENTRADAS"

                    @ 5,5 SAY "INTRODUCE CLIENTE" GET WPROV
                    READ
                    Wprov = UPPER(Wprov)
                    wprov=trim(wprov)

             &A
                    SEEK WPROV
                    BROW FIEL NOMBRE,POBLACION NOMENU NOAPPEND NOEDIT NODELETE
                    CLEAR

                    RESP = "s"
                    @ 2,2 SAY "ENCONTRADO S/N" GET RESP
                    READ
                    RESP = LOWER(RESP)

      IF RESP="s"
                               WCLAVE  = CLAVE
                               WNOMBRE = NOMBRE


        USE
        STORE SPACE(10) TO WNUMFAC




        CLEAR


        @ 5,5 SAY "INTRODUCE Nº DE FACTURA" GET WNUMFAC

        READ

             &C
        WBUSQUEDA=WCLAVE+WNUMFAC
        SEEK WBUSQUEDA
        IF FOUND()
             CLEAR
             RESP="n"







             @ 3,5  say "Proveedor: " +WNOMBRE
             @ 4,5  say "NUM. FACT. " +WNUMFAC


             @ 6,5  SAY "¿ESTA SEGURO DE QUE DESEA BORRARLA?" GET RESP

             READ
             RESP=LOWER(RESP)
             IF RESP="s"
                  DELETE
             ENDIF


          else
           clear
           @ 5,5 say "NO SE ENCUENTRA LA FACTURA"
           WAIT window

          ENDIF
          use
       CASE CON="v"
       RETURN
      ENDCASE

ENDdo

RETURN



******************************************************
*******PROCEDIMIENTO LISTADO***************
*************************LISTADO POR PROVEEDOR ANUAL/MENSUAL

PROCEDURE LISTADO
                    CLEAR
                    STORE SPACE(40) TO WPROV

                    @ 5,5 SAY "INTRODUCE PROVEEDOR" GET WPROV
                    READ
                    Wprov = UPPER(Wprov)
                    wprov=trim(wprov)

             &A
                    SEEK WPROV
                    BROW FIEL NOMBRE,POBLACION NOMENU NOAPPEND NOEDIT NODELETE
                    CLEAR

                    RESP = "s"
                    @ 2,2 SAY "ENCONTRADO S/N" GET RESP
                    READ
                    RESP = LOWER(RESP)

      IF RESP="s"
                               WCLAVE  = CLAVE
                               WNOMBRE = NOMBRE


           CLEAR
           RESP="a"
           @ 5,5 SAY "LISTADO (A)NUAL O (M)ENSUAL" GET RESP
           READ
           RESP=LOWER(RESP)
           DO CASE


                CASE RESP="a"
                          CLEAR
                          WANO=2000
                          @ 5,5 SAY "ESCRIBA EL AÑO: " GET WANO
                          READ

             &C


                          SEEK WCLAVE
                                SET PRINT ON PROMPT
                                ?"LISTADO ANUAL DE "+WNOMBRE+" "+STR(WANO,4)
                                ?"-------------------------------------------"
                                ?
                                WTOTAL=0
                                DO WHILE .NOT.EOF()
                                   IF WANO=YEAR(FECHA).AND.WCLAVE=CLAVE
                                     ?NUMFAC,FECHA,IMPORTE,PAGADO,REFAG1,REFAG2,REFAG3,REFAG4,REFAG5,REFAG6
                                     WTOTAL=WTOTAL+IMPORTE
                                   ENDIF

                                   SKIP

                                ENDDO
                                ?
                                ?"TOTAL: ",WTOTAL,"EUR"
                                SET PRINT OFF
                                SET PRINTER TO
                                WAIT window
                CASE RESP="m"
                          WANO=2000
                          WMES=1
                          CLEAR
                          @ 5,5 SAY "ESCRIBE EL AÑO: " GET WANO
                          @ 6,5 SAY "ESCRIBE EL MES: " GET WMES

                          READ


             &C



                          SEEK WCLAVE
                                SET PRINT ON PROMPT
                                ?"LISTADO DE "+WNOMBRE+" "+STR(WANO,4)+" "+CMONTH(CTOD("01-"+STR(WMES,2)+"-2000"))
                                ?"-------------------------------------------"
                                ?
                                WTOTAL=0
                                DO WHILE .NOT.EOF()
                                   IF WANO=YEAR(FECHA).AND.WMES=MONTH(FECHA).AND.WCLAVE=CLAVE


                                     ?NUMFAC,FECHA,IMPORTE,PAGADO,REFAG1,REFAG2,REFAG3,REFAG4,REFAG5,REFAG6
                                     WTOTAL=WTOTAL+IMPORTE
                                   ENDIF
                                     SKIP
                                ENDDO
                                ?
                                ?"TOTAL: ",WTOTAL,"EUR"
                                SET PRINT OFF
                                SET PRINTER TO
                             WAIT window

                OTHERWISE
                          RETURN


           ENDCASE






      ENDIF

RETURN

************************************************
********PROCEDIMIENTO LISMEN*********************
********************LISTADO MENSUAL DE PROVEEDOR PAGADO/NO
********************ORDENADO POR PROVEEDOR CON TOTAL

PROCEDURE LISMEN












RETURN


***********************************************************
**********PROCEDIMIENTO LISAGR*****************************
***************************LISTADO AGRUPADO ANUAL/MENSUAL
***************************CON TOTAL
*********************************************
PROCEDURE LISAGR
      clear
      wmes=1
      wano=2000
      @ 5,5 say "Mes en numero: " get wmes
      @ 6,5 say "Año:           " get wano
      read
      *SELECT 1
      &C
      *SELECT 2
      *USE \\BELEN\C\PROVEEDO\PROVEEDORES INDEX \\BELEN\C\PROVEEDO\IN1CLAVE




      *SELECT 1
            go top
            wtotal=0
            store space(10) to wclave
            do while .not.eof()
                              wclave=clave



                              wimporte=0
                              do while wclave=clave
                                  if (wano=year(fecha)).and.(wmes=month(fecha))
                                     wimporte=wimporte+importe
                                  ENDIF
                                  SKIP
                              enddo

                              if wimporte<>0
                                
                                ?WCLAVE,wimporte


                              endif
                              wtotal=wtotal+wimporte



            enddo
            ?"Total:  " ,wtotal

       WAIT window

RETURN



***********************************************************
**********PROCEDIMIENTO SINCRO*****************************
********************************SINCRONIZACION CON PAGOS
********************************DEL FICHERO PROVEEDORES EN AMBOS
***********************************************************

PROCEDURE SINCRO

 VALOR=.F.
 IF VALOR
RUN COPY C:\PAGOS\PROVEEDO.DBF C:\AUXILIAR
RUN COPY C:\PAGOS\IN1CLAVE.NDX C:\AUXILIAR
RUN COPY C:\PAGOS\INPROV.NDX C:\AUXILIAR


RUN REN C:\AUXILIAR\PROVEEDO.DBF BDAUX.DBF
RUN REN C:\AUXILIAR\IN1CLAVE.NDX INAUX1.NDX
RUN REN C:\AUXILIAR\INPROV.NDX INAUX2.NDX


RUN COPY C:\AUXILIAR\BDAUX.DBF C:\PROVEEDO
RUN COPY C:\AUXILIAR\INAUX1.NDX C:\PROVEEDO
RUN COPY C:\AUXILIAR\INAUX2.NDX C:\PROVEEDO


    store space(10) to wclave1
    SELECT 1
    USE C:\PROVEEDO\BDAUX INDEX INAUX1,INAUX2
    SELECT 2
    USE C:\PROVEEDO\PROVEEDORES INDEX IN1CLAVE,INPROV
    SELECT 2
    GO TOP
   DO WHILE .NOT.EOF()
    select 2
    WCLAVE1=CLAVE  && cogemos una clave de bd2
    WCLAVE1=TRIM(WCLAVE1)
    SELECT 1
    SEEK WCLAVE1   && miramos si esta en bd1

    IF NOT.FOUND() && si no esta

          select 1  && la a¤adimos a bd1
         APPEND BLANK

            store space(40) to wnombre
            store space(40) to wdireccion
            store space(6)  to wcodpostal
            store space(30) to wpoblacion
            store space(10) to wprovincia
            store space(10) to wpais
            store space(16) to wnif
            store space(10) to wclave
            store space(40) to wdepartamen
            store space(40) to wpoligono
            store space(16) to wtelefono
            store space(10) to wapdo
            store space(10) to wcontacto
            store space(12) to wactividad
            store space(50) to wformapago
            store space(15) to wbanco
            store space(30) to wn_cuenta

         select 2  && recogemos los datos del registro en bd2
            wnombre=NOMBRE
            wdireccion=DIRECCION
            wcodpostal=CODPOSTAL
            wpoblacion=POBLACION
            wprovincia=PROVINCIA
            wpais=PAIS
            wnif=NIF
            wclave=CLAVE
            wdepartamen=DEPARTAMEN
            wpoligono=POLIGONO
            wtelefono=TELEFONO
            wapdo=APDO
            wcontacto=CONTACTO
            wactividad=ACTIVIDAD
            wformapago=FORMAPAGO
            wbanco=BANCO
            wn_cuenta=N_CUENTA
         select 1 && los a¤adimos a bd1

           REPLACE NOMBRE WITH WNOMBRE
           REPLACE DIRECCION WITH WDIRECCION
           REPLACE CODPOSTAL WITH WCODPOSTAL
           REPLACE POBLACION WITH WPOBLACION
           REPLACE PROVINCIA WITH WPROVINCIA
           REPLACE PAIS WITH WPAIS
           REPLACE NIF WITH WNIF
           REPLACE CLAVE WITH WCLAVE
           REPLACE DEPARTAMEN WITH WDEPARTAMEN
           REPLACE POLIGONO WITH WPOLIGONO
           REPLACE TELEFONO WITH WTELEFONO
           REPLACE APDO WITH WAPDO
           REPLACE CONTACTO WITH WCONTACTO
           REPLACE ACTIVIDAD WITH WACTIVIDAD
           REPLACE FORMAPAGO WITH WFORMAPAGO
           REPLACE BANCO WITH WBANCO
           REPLACE N_CUENTA WITH WN_CUENTA
           select 2
           skip
        else
           select 2
           skip


       ENDIF
   ENDDO



   ****invertimos el proceso

    SELECT 1
    GO TOP
   DO WHILE .NOT.EOF()
    select 1
    WCLAVE1=CLAVE  && cogemos una clave de bd1
    SELECT 2
    SEEK WCLAVE1   && miramos si esta en bd2

    IF NOT.FOUND() && si no esta

          select 2  && la añadimos a bd2
         APPEND BLANK

            store space(40) to wnombre
            store space(40) to wdireccion
            store space(6)  to wcodpostal
            store space(30) to wpoblacion
            store space(10) to wprovincia
            store space(10) to wpais
            store space(16) to wnif
            store space(10) to wclave
            store space(40) to wdepartamen
            store space(40) to wpoligono
            store space(16) to wtelefono
            store space(10) to wapdo
            store space(10) to wcontacto
            store space(12) to wactividad
            store space(50) to wformapago
            store space(15) to wbanco
            store space(30) to wn_cuenta

         select 1  && recogemos los datos del registro en bd1
            wnombre=NOMBRE
            wdireccion=DIRECCION
            wcodpostal=CODPOSTAL
            wpoblacion=POBLACION
            wprovincia=PROVINCIA
            wpais=PAIS
            wnif=NIF
            wclave=CLAVE
            wdepartamen=DEPARTAMEN
            wpoligono=POLIGONO
            wtelefono=TELEFONO
            wapdo=APDO
            wcontacto=CONTACTO
            wactividad=ACTIVIDAD
            wformapago=FORMAPAGO
            wbanco=BANCO
            wn_cuenta=N_CUENTA
         select 2 && los a¤adimos a bd2

           REPLACE NOMBRE WITH WNOMBRE
           REPLACE DIRECCION WITH WDIRECCION
           REPLACE CODPOSTAL WITH WCODPOSTAL
           REPLACE POBLACION WITH WPOBLACION
           REPLACE PROVINCIA WITH WPROVINCIA
           REPLACE PAIS WITH WPAIS
           REPLACE NIF WITH WNIF
           REPLACE CLAVE WITH WCLAVE
           REPLACE DEPARTAMEN WITH WDEPARTAMEN
           REPLACE POLIGONO WITH WPOLIGONO
           REPLACE TELEFONO WITH WTELEFONO
           REPLACE APDO WITH WAPDO
           REPLACE CONTACTO WITH WCONTACTO
           REPLACE ACTIVIDAD WITH WACTIVIDAD
           REPLACE FORMAPAGO WITH WFORMAPAGO
           REPLACE BANCO WITH WBANCO
           REPLACE N_CUENTA WITH WN_CUENTA
           select 1
           skip
        else
           select 1
           skip


       ENDIF
   ENDDO
CLOSE INDEX
CLOSE DATABASES
ENDIF && DE VALOR

RETURN
