c
c       program main
c
c        implicit none
c
c        integer i, j
c        integer u
c
c        integer arrayTest1(30)
c        double precision matrixTest(10, 10)
c
c                           
c        DO 666, i = 1, 30
c            arrayTest1(i) = i
c666     CONTINUE
c
c        call wOutIntegerVector(arrayTest1, 30, 
c     >                        'output.txt',LEN('output.txt'),
c     >                        'array 1', LEN('array 1'))
c
c        DO 20, i = 1, 10
c            DO 30, j = 1, 10
c                  matrixTest(i, j) = 0
c30          CONTINUE
c20      CONTINUE
c
c        call wOutDoubleMatrix(matrixTest, 10, 10, 
c     >                   'output.txt', LEN('output.txt'),
c     >                   'matrix 1', LEN('matrix 1'))
c
c        DO 66, i = 1, 10
c            DO 77, j = 1, 10
c                  matrixTest(i, j) = 1
c77          CONTINUE
c66      CONTINUE
c
c        call wOutDoubleMatrix(matrixTest, 10, 10, 
c     >                   'output.txt', LEN('output.txt'),
c     >                   'matrix 2', LEN('matrix 2'))
c
c       return 
c
c        end

       subroutine wOutIntegerVar(variable, 
     >                           u,
     >                           variableName, 
     >                           variableNameSIZE)

             integer variableNameSIZE

             character(variableNameSIZE) variableName

             integer u

             integer variable
 
             !open(u, FILE = fileName, STATUS = 'OLD')
             
             write (u,'(*(G0.6,:,"'//achar(9)//'"))') variableName
 
             write (u,'(*(G0.6,:,"'//achar(9)//'"))') variable
 
             !close(1)

        end


        subroutine wOutDoubleVar(variable, 
     >                           u,
     >                           variableName, 
     >                           variableNameSIZE)
             integer u
             
             integer variableNameSIZE

             character(variableNameSIZE) variableName

             double precision variable
 
             !open(1, FILE = fileName, ACCESS = 'APPEND', STATUS = 'OLD')
             
             write (u,'(*(G0.6,:,"'//achar(9)//'"))') variableName
 
             write (u,'(*(G0.6,:,"'//achar(9)//'"))') variable
 
             !close(1)

        end

        subroutine wOutIntegerVector(array, length, 
     >                               u,
     >                               variableName, 
     >                               variableNameSIZE)

             integer u

             integer variableNameSIZE

             character(variableNameSIZE) variableName
 
             integer length
             
             integer array(length)
 
             !open(1, FILE = fileName, ACCESS = 'APPEND', STATUS = 'OLD')
             
             write (u,'(*(G0.6,:,"'//achar(9)//'"))') variableName
 
             write (u,'(*(G0.6,:,"'//achar(9)//'"))') array
 
             !close(1)

        end

        subroutine wOutDoubleVector(array, length, 
     >                              u,  
     >                              variableName, 
     >                              variableNameSIZE)

             integer u

             integer variableNameSIZE

             character(variableNameSIZE) variableName
 
             integer length
             
             real array(length)
 
             !open(1, FILE = fileName, ACCESS = 'APPEND', STATUS = 'OLD')
             
             write (u,'(*(G0.6,:,"'//achar(9)//'"))') variableName
 
             write (u,*) array
 
             !close(1)

        end

        subroutine wOutIntegerMatrix(matrix, nrow, ncol, 
     >                               u,  
     >                               variableName, 
     >                               variableNameSIZE)

            integer u

            integer variableNameSIZE
            character(variableNameSIZE) variableName

            integer nrow
            integer ncol
            
            integer matrix(nrow, ncol)

            integer lineCopy(ncol)

            integer i, j

            !open (1, FILE = fileName, ACCESS = 'APPEND', STATUS = 'OLD')
            
            write (u,'(*(G0.6,:,"'//achar(9)//'"))') variableName

            DO 102, i = 1, nrow
                DO 103, j = 1, ncol
                     lineCopy(j) = matrix(i, j)
103             CONTINUE
                write (u,'(*(G0.6,:,"'//achar(9)//'"))') lineCopy
102         CONTINUE

            !close(1)    

        end 

        subroutine wOutDoubleMatrix(matrix, nrow, ncol, 
     >                              u,
     >                              variableName, 
     >                              variableNameSIZE)
             
            integer u

            integer variableNameSIZE
            character(variableNameSIZE) variableName

            integer nrow
            integer ncol
            
            real matrix(nrow, ncol)

            real lineCopy(ncol)

            integer i, j

            !open (1, FILE = fileName, ACCESS = 'APPEND', STATUS = 'OLD')
            
            write (u,'(*(G0.6,:,"'//achar(9)//'"))') variableName

            DO 102, i = 1, nrow
                DO 103, j = 1, ncol
                     lineCopy(j) = matrix(i, j)
103             CONTINUE
                write (u,'(*(G0.6,:,"'//achar(9)//'"))') lineCopy
102         CONTINUE

            !close(1)    

        end 