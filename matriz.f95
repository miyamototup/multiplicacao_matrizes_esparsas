program matriz
use subrotinas

!Leitura as colunas
ncol=16
ntotal=497
lfinal=35
linicial=5
allocate(colunas(497))
call lc(ncol,ntotal,lfinal,linicial,colunas)
colunas(497)=colunas(496)

!!Leitura as linhas
ncol=20
ntotal=49920
lfinal=2532
linicial=37
allocate(linhas(49920))
call ll(ncol,ntotal,lfinal,linicial,linhas)

!Leitura dos Elementos
ncol=5
ntotal=49920
lfinal=12516
linicial=2533
allocate(elementos(49920))
call le(ncol,ntotal,lfinal,linicial,elementos)

allocate(u(496),v1(496),v2(496),A(496,496))
v1=0
v2=0
A=0
call random_seed
call random_number(u)

!Produto entre a matriz A e o vetor de números aleatórios
!obtido sem montar a matriz, passando apenas, linhas, colunas,
!elementos e aleatorio, ou seja utilizando armazenamento CCS
call m1(linhas, colunas, elementos,u,v1)
print*,"Tempo para calcular o produto utilizando armazenamento CCS:",tp2-tp1

!Matmul
!Monta a matriz A a partir dos vetores: linhas, colunas e elementos,
!multiplica pelo vetor de números aleatórios e armazena o resultado em 
!r2. 
call m2(linhas, colunas, elementos,u,v2,A)
print*,"Tempo para calcular o produto utilizando o formato usual:",tp2-tp1

!print*,v1
!print*,v2

end program matriz
