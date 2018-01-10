//fifo memory in chisel
package fifo

import Chisel._

object fifoDefines{
	val WIDTH = 32

}

class fifoIO extends Bundle {
	val enq			= Decoupled(UInt(width = fifoDefines.WIDTH)).flip
	val deq			= Decoupled(UInt(width = fifoDefines.WIDTH))
	val notFull		= Bool(OUTPUT)
	val notEmpty	= Bool(OUTPUT)
	val clear 		= Bool(INPUT)

	enq.ready	   := notFull 
	deq.valid	   := notEmpty 

}

//fifo with a depth of 1
/*class fifo1 extends Module{
	val io = new fifoIO

	val dReg = RegInit(UInt(5, width = fifoDefines.WIDTH))
	val notEmptyReg = RegInit(Bool(false))

	io.deq.bits := dReg
	io.notEmpty := notEmptyReg
	io.notFull	:= !notEmptyReg

	when(io.clear){
		notEmptyReg   := Bool(false)
	}. otherwise{
		when(!notEmptyReg && io.enq.valid){
			notEmptyReg	  := Bool(true)
		}. elsewhen(io.deq.ready && notEmptyReg){
			notEmptyReg	  := Bool(false)
		}
	}	
	when(io.enq.valid && !notEmptyReg){
		dReg		  := io.enq.bits
	}
}*/


//a fifo with parameterized depth

class fifoF(depth : Int) extends Module{
	require(isPow2(depth),"fifoF depth must be power of 2")
	val io = new fifoIO
	val size = log2Up(depth)
	val depthReg  = RegInit(UInt(depth, width = fifoDefines.WIDTH))

	val rd_ptr	= RegInit(UInt(0, width = size + 1))
	val wr_ptr  = RegInit(UInt(0, width = size + 1))
	val rd_addr = rd_ptr(log2Up(depth)-1,0)
	val wr_addr	= wr_ptr(log2Up(depth)-1,0)
	val notFull   = RegInit(Bool(true))
	val notEmpty  = RegInit(Bool(false))

	//conditions for full and empty
	notFull    := !(wr_ptr === depthReg)
	notEmpty   := !(rd_ptr === wr_ptr)

	val mem 	= Mem(UInt(width = fifoDefines.WIDTH),depth) 

	//conditions for enqueue and dequeue
	val enq_condn 	= io.enq.valid && notFull
	val deq_condn 	= io.deq.ready && notEmpty
	
	io.deq.bits 	:= mem(rd_addr)
	io.notFull		:= notFull
	io.notEmpty 	:= notEmpty 

	when(io.clear || !notEmpty){
		rd_ptr 		:= UInt(0)
		wr_ptr		:= UInt(0)
	}. otherwise{
		when(enq_condn){
			mem(wr_addr) := io.enq.bits
			wr_ptr    := wr_ptr + UInt(1)
		}. elsewhen(deq_condn){
			io.deq.bits := mem(rd_addr)
			rd_ptr 	    := rd_ptr + UInt(1)
		}	
	}	
}


class fifoFTests(c: fifoF) extends Tester(c){
	//check the state of the fifo
	//peek(c.notEmptyReg)
	peek(c.io.notFull)
	peek(c.io.notEmpty)
	println("enq.ready:"+peek(c.io.enq.ready)+
			" deq.valid:"+peek(c.io.deq.valid)+
			" deq.bits:"+peek(c.io.deq.bits)
			)

	//step(1)
	
	//enqueue the fifo
	poke(c.io.enq.valid,1)
	poke(c.io.enq.bits,20)
	poke(c.io.deq.ready,0)
	step(1)
	peek(c.io.notFull)
	peek(c.io.notEmpty)
	peek(c.mem(0))
	println("enq.ready:"+peek(c.io.enq.ready)+
		" deq.valid:"+peek(c.io.deq.valid)+
		" deq.bits:"+peek(c.io.deq.bits)
		)

	/*poke(c.io.enq.valid,1)
	poke(c.io.enq.bits,30)
	poke(c.io.deq.ready,0)
	step(1)
	//peek(c.notEmptyReg)
	//peek(c.dReg)
	peek(c.io.notFull)
	peek(c.io.notEmpty)
	println("enq.ready:"+peek(c.io.enq.ready)+
		" deq.valid:"+peek(c.io.deq.valid)+
		" deq.bits:"+peek(c.io.deq.bits)
		)

	poke(c.io.enq.valid,1)
	poke(c.io.enq.bits,35)
	poke(c.io.deq.ready,0)
	step(1)
	//peek(c.notEmptyReg)
	//peek(c.dReg)
	peek(c.io.notFull)
	peek(c.io.notEmpty)
	println("enq.ready:"+peek(c.io.enq.ready)+
		" deq.valid:"+peek(c.io.deq.valid)+
		" deq.bits:"+peek(c.io.deq.bits)
		)			
	
	poke(c.io.enq.valid,1)
	poke(c.io.enq.bits,10)
	poke(c.io.deq.ready,0)
	step(1)
	//peek(c.notEmptyReg)
	//peek(c.dReg)
	peek(c.io.notFull)
	peek(c.io.notEmpty)
	println("enq.ready:"+peek(c.io.enq.ready)+
		" deq.valid:"+peek(c.io.deq.valid)+
		" deq.bits:"+peek(c.io.deq.bits)
		)

	poke(c.io.enq.valid,1)
	poke(c.io.enq.bits,51)
	poke(c.io.deq.ready,0)
	step(1)
	//peek(c.notEmptyReg)
	//peek(c.dReg)
	peek(c.io.notFull)
	peek(c.io.notEmpty)
	println("enq.ready:"+peek(c.io.enq.ready)+	
		" deq.valid:"+peek(c.io.deq.valid)+
		" deq.bits:"+peek(c.io.deq.bits)
		)


	//dequeue the fifo
	poke(c.io.deq.ready,1)
	step(1)
	//peek(c.notEmptyReg)
	peek(c.io.notFull)
	peek(c.io.notEmpty)
	println("enq.ready:"+peek(c.io.enq.ready)+
		" deq.valid:"+peek(c.io.deq.valid)+
		" deq.bits:"+peek(c.io.deq.bits)
		)

	//enqueue the fifo
	/*poke(c.io.enq.valid,1)
	poke(c.io.enq.bits,300)
	poke(c.io.deq.ready,0)
	step(1)
	peek(c.notEmptyReg)
	peek(c.dReg)
	peek(c.io.notFull)
	peek(c.io.notEmpty)	
	println("enq.ready:"+peek(c.io.enq.ready)+
		" deq.valid:"+peek(c.io.deq.valid)+
		" deq.bits:"+peek(c.io.deq.bits)
		)*/

	
	poke(c.io.deq.ready,1)
	step(1)
	//peek(c.notEmptyReg)
	peek(c.io.notFull)
	peek(c.io.notEmpty)
	println("enq.ready:"+peek(c.io.enq.ready)+
		" deq.valid:"+peek(c.io.deq.valid)+
		" deq.bits:"+peek(c.io.deq.bits)
		)

	//clear the fifo
	poke(c.io.clear,1)
	step(1)
	println("enq.ready:"+peek(c.io.enq.ready)+
		" deq.valid:"+peek(c.io.deq.valid)+
		" deq.bits:"+peek(c.io.deq.bits)
		)*/
}

object fifoF {
  def main(args: Array[String]): Unit = {
    val tutArgs = args.slice(1, args.length)
    chiselMainTest(tutArgs, () => Module(new fifoF(4))) {
      c => new fifoFTests(c) }
  }
}




/*class fifo1Tests(c: fifo1) extends Tester(c){
	//check the state of the fifo
	peek(c.notEmptyReg)
	peek(c.io.notFull)
	peek(c.io.notEmpty)
	println("enq.ready:"+peek(c.io.enq.ready)+
			" deq.valid:"+peek(c.io.deq.valid)+
			" deq.bits:"+peek(c.io.deq.bits)
			)

	//step(1)
	
	//enqueue the fifo
	poke(c.io.enq.valid,1)
	poke(c.io.enq.bits,20)
	poke(c.io.deq.ready,0)
	step(1)
	peek(c.notEmptyReg)
	peek(c.dReg)
	peek(c.io.notFull)
	peek(c.io.notEmpty)
	println("enq.ready:"+peek(c.io.enq.ready)+
		" deq.valid:"+peek(c.io.deq.valid)+
		" deq.bits:"+peek(c.io.deq.bits)
		)	
	//dequeue the fifo
	poke(c.io.deq.ready,1)
	step(1)
	peek(c.notEmptyReg)
	peek(c.io.notFull)
	peek(c.io.notEmpty)
	println("enq.ready:"+peek(c.io.enq.ready)+
		" deq.valid:"+peek(c.io.deq.valid)+
		" deq.bits:"+peek(c.io.deq.bits)
		)

	//enqueue the fifo
	poke(c.io.enq.valid,1)
	poke(c.io.enq.bits,300)
	poke(c.io.deq.ready,0)
	step(1)
	peek(c.notEmptyReg)
	peek(c.dReg)
	peek(c.io.notFull)
	peek(c.io.notEmpty)	
	println("enq.ready:"+peek(c.io.enq.ready)+
		" deq.valid:"+peek(c.io.deq.valid)+
		" deq.bits:"+peek(c.io.deq.bits)
		)

	//dequeue the fifo
	poke(c.io.deq.ready,1)
	step(1)
	peek(c.notEmptyReg)
	peek(c.io.notFull)
	peek(c.io.notEmpty)
	println("enq.ready:"+peek(c.io.enq.ready)+
		" deq.valid:"+peek(c.io.deq.valid)+
		" deq.bits:"+peek(c.io.deq.bits)
		)

	//clear the fifo
	poke(c.io.clear,1)
	step(1)
	println("enq.ready:"+peek(c.io.enq.ready)+
		" deq.valid:"+peek(c.io.deq.valid)+
		" deq.bits:"+peek(c.io.deq.bits)
		)
}*/

/*object fifo1 {
  def main(args: Array[String]): Unit = {
    val tutArgs = args.slice(1, args.length)
    chiselMainTest(tutArgs, () => Module(new fifo1())) {
      c => new fifo1Tests(c) }
  }
}*/
