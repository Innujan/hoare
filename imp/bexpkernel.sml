structure BexpKernel = struct
	open Datatypes
	open Util

	datatype BexpKernel = BexpKernel of {
		dim : string list,
		voxels : bool list
	}
	
	val BexpKernelSize : int = 10
	
	fun flipBitsList ([]) = [] | flipBitsList (v::vs) = (not v) :: flipBitsList(vs)
	
	fun orBitList ([], []) = [] | orBitList (x::xs, y::ys) = (x or y) :: orBitList(xs,ys)
	
	fun notBexpKernel (BexpKernel {dim = dim, voxels = v}) = BexpKernel {dim = dim, voxels = flipBitsList(v)}
	fun orBexpKernel (BexpKernel {dim = d1, voxels = x}, BexpKernel {dim = d2, voxels = y}) = BexpKernel {dim = dim, voxels = flipBitsList(v)}
	
	fun insertSorted (s, lst) =
		let
			fun aux ([], idx, acc) = (List.rev (s::acc), idx)
			  | aux (x::xs, idx, acc) =
					if String.compare(s, x) = LESS then
						(List.rev (s::acc) @ (x::xs), idx)
					else
						aux(xs, idx+1, x::acc)
		in
			aux(lst, 0, [])
		end
	
	
	fun expandBexpKernel ([], BexpKernel {dim = olddim, voxels = v}) = 
		BexpKernel {dim = olddim, voxels = v}
	  | expandBexpKernel (d::ds, BexpKernel {dim = olddim, voxels = v}) =
		if List.exists (fn x => x = d) olddim then
			expandBexpKernel(ds, BexpKernel {dim = olddim, voxels = v})
		else
			let
				val (newDim, location) = insertSorted(d, olddim)
				val blockSize = Int.pow(BexpKernelSize, location)
				val repeatFactor = Int.pow(BexpKernelSize, length olddim - location)

				fun repeatBlocks ([], _, _) = []
				  | repeatBlocks (lst, blkSize, rep) =
						List.concat (List.tabulate(rep, fn _ =>
							List.concat (List.tabulate(length lst div blkSize, fn i =>
								List.take(List.drop(lst, i*blkSize), blkSize)
							))
						))

				val newVoxels = repeatBlocks(v, blockSize, repeatFactor)
			in
				expandBexpKernel(ds, BexpKernel {dim = newDim, voxels = newVoxels})
			end
			
	
	fun makeBexpKernel(b: Bexp) =
		case b of
			boolean b => BexpKernel { dim = [], voxels = [b] }
		  | imply (b1,b2) =>
				let
					val i1 = makeBexpKernel(b1)
					val i2 = makeBexpKernel(b2)
					val BexpKernel { dim = l1, voxels = _ } = i1
					val BexpKernel { dim = l2, voxels = _ } = i2
					val dim = fn (l1,l2) => (sortString (List.foldl (fn (x,acc) => if List.exists (fn y => y=x) acc then acc else x::acc) (l1@l2) []))
					val k1 = expandBexpKernel(dim, i1)
					val k2 = expandBexpKernel(dim, i2)
				in
					orBexpKernal(notBexpKernel (i1), i2)
				end
			(*less and equal need to be defined*)
	
end