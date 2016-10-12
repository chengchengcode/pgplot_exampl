
module vars
                integer,parameter     ::      LENTH=100000
end module vars 
!习惯了，还是定义个module

program plot	
        use vars
	implicit none

	integer i,j,iostat,status,NUMB,JUMP_L
!	数数组里数字个数的一点参数
	real	line_x(lenth)
	real	line_y(lenth)
	real	test_2(lenth,lenth)
!	要被画图的x，y
	character(len=80) XX,YY,listname1,listname2,FILENAME,iflog,IFPOINT
!	横纵坐标的名字
	real ytemp,x_JUMP,y1_JUMP,y2_JUMP,y3_JUMP,&
		y4_JUMP,y5_JUMP,y6_JUMP,y7_JUMP,y8_jump,y9_jump
!	临时变量
	real	x_left,x_right,y_up,y_down
!	坐标框架的左右上下数字
        real            small,large
        external        small,large
!	两个胡定义的函数

!	write(*,*)'file name:'
!	pause

!	the rest will be open after sucessfully read

	call getarg(1,FILENAME)
!!	call getarg(2,IFLOG)
!
!	IFLOG='unknown'
!	IFPOINT='unknow'
!
!do while((IFLOG .ne. 'log') .and.(IFLOG .ne. 'normal'))
!		write(*,*)'log or normal cooradinate? Type log/normal:'
!		read(*,*)IFLOG
!end do
!
!do while((IFPOINT .ne. 'point') .and.(IFPOINT .ne. 'line'))
!	write(*,*)'point or line? Type point/line:'
!	read(*,*)IFpoint
!end do
!!
IFLOG='normal'
IFPOINT='line'
!

	open(unit=11,file=trim(FILENAME))
!	write(*,*)'log ?'
!	假设fort.11里有两列数，单精度的


do i=1,5,1
	do j=1,5,1
		read(11,*,iostat=status)ytemp! test_2(i,j)
		write(*,*)ytemp,status
		if(status<0)then
			exit
		end if
	end do
	write(*,*)ytemp
	
end do

	write(*,*)test_2
	stop



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!这部分用于跳行读数，比如要跳过前JUMP_L行，通常取零也就是从第一行开始读
	JUMP_L=0
	do	i=1,100000,1
		if(i<JUMP_L)then
			read(11,*,iostat=status)x_JUMP,y1_JUMP,y2_JUMP,y3_JUMP,&
			y4_JUMP!,y5_JUMP,y6_JUMP,y7_JUMP
			cycle
		end if
		read(11,*,iostat=status)line_x(i-JUMP_L),line_y(i-JUMP_L)
!	读入fort.11的两列数到line_x，line_y里
	  	if(status<0)then
			NUMB=i-1
			exit
		end if
	enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	NUMB=NUMB-JUMP_L	
!	这时得到line_x,line_y的数组大小
	XX='XXXXXXXXXXX'
	YY='YYYYYYYYYYY'
!	x,y的名字
	call pgbegin(0,'?',1,1)
!	pgbegin是说开始pgplot做图了，声明一下，？表示过会手工选择画图格式
!	或者：
!	如果需要生成ps
!	call pgbegin(0,'name.ps/cps',1,1)
!	直接打屏
!	call pgbegin(0,'/xw',1,1)
	call pgslw(3)
!	pgslw是说线宽是3， set line width
	call pgsls(1)
!	pgsls是说划线的类型，1是实线，之后是各种虚，set line style
	call pgscf(2) 
!	pgscf是设定字符串的字体，2是罗马字体， set character font
        call pgvport(0.2,0.8,0.2,0.8)
!	pgvport的概念需要解释一下：
!	pgplot认为一张图画在一个长宽都是1的方块里
!	这里的第一个0.2，0.8是指x轴从0.2的地方开始到0.8为止，是用来画图的
!	这里的第二个0.2，0.8是指y轴从0.2的地方开始到0.8为止，是用来画图的
!	以后也许会用 call pgvport(0.1,0.45,0.2,0.5)之后画图，再之后
!	call pgvport(0.55,0.9,0.2,0.5)，来实现两张图并排
	x_left=small(line_x,numb)-0.1*(large(line_x,numb)-small(line_x,numb))
	x_right=large(line_x,numb)+0.1*(large(line_x,numb)-small(line_x,numb))
	y_down=small(line_y,numb)-0.1*(large(line_y,numb)-small(line_y,numb))
	y_up=large(line_y,numb)+0.1*(large(line_y,numb)-small(line_y,numb))
!	这是我写的间距控制变量，也就是向左向右向上向下都露出来一些空间
        call pgwindow(x_left,x_right,y_down,y_up)
!	pgwindow是指定画出来的坐标方框的上下左右位置，可以直接写上实数，我这里懒得每次都写了
	if(trim(IFLOG) .eq. 'log')then
        call pgbox('cblnts',0.0,0,'cblnvt2s',0.0,0)
	else if(trim(IFLOG) .eq. 'normal')then
        call pgbox('cbnts',0.0,0,'cbnvt2s',0.0,0)
	else
		write(*,*)'log or normal cooradinate? exit'
		stop
	end if
!	pgbox参数较多，现在看到的这几个
!	b是说画一个十字的坐标轴
!	c是说把刚才b得到的坐标轴画成一个封闭的坐标框
!	n是说坐标轴上要标数字
!	t是说比较整的刻度画大些
!	s是说比较零碎的刻度画小些
!	用对数坐标可以加l
!.....
	call pglabel(XX,YY,'TITLE: IF NEEDED')
!	pglabel是把x，y坐标轴名字标上，x的还好，y的通常会被文字挡着所以要再
	call pgptxt(x_left,0.5*y_down+0.5*y_up,90.0,0.5,YY)
!	这里要手动调节一下，用pgptxt直接在图上写字
!	前两个参数是最后一个字符串YY的x，y坐标，90.0是转90度
!	当然也可以不调整，直接用pglable
!	不需要字符串的话用‘’表示空
!	call pglabel(XX,'','')
	
	if(trim(ifpoint) .eq. 'line')then
		call pgline(NUMB,line_x,line_y)
	else if(trim(ifpoint) .eq. 'point')then
		call pgpoint(NUMB,line_x,line_y,17)
	else
		write(*,*)'point or line? exit'
		stop
	end if	
	
!	pgline的参数依次是，需要画的点的个数，点的x坐标数组，点的y坐标数组
!	call pgpoint(NUMB,line_x,line_y,17)
!	pgpoint是只把点点出来，不连线，最后一个参数是点的形状，17是圆点

!	可以随时call pgsls之类的改变线的类型
!	pgsci改变颜色,pgpoly可以画阴影
!	字符串用\u表示上标\d表示下标，这么说不太确切
!	比如x_1, 写作：x\d1\u, x平方是 x\u2\d
!	需要什么bt符号了pgplot说明书上有，\(n)
!	\A是A上一个圈的那个angstrom

	call pgend
!	最后用pgend结束
end program plot



function small(x,numb)
	use vars
	implicit none
	real    x(LENTH),small
	integer numb,i
	small=x(1)
	do i=1,numb,1
!        write(*,*)i
        if(small>x(i))then
			small = x(i)
        end if
	end do

	return
end function small

function large(x,numb)
	use vars
	implicit none
	real    x(LENTH),large
	integer numb,i

	large=x(1)
	do i=1,numb,1
!		write(*,*)i
        if(large<x(i))then
        	large = x(i)
        end if
	end do

	return
end function large

subroutine CHAR2NUM(XXX,NUM)
	use vars
	implicit none
	character(len=80) XXX,YY
	integer	num
	read(XXX,*)NUM
	write(*,*)NUM
	return
end subroutine CHAR2NUM

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

