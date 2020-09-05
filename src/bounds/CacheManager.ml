open Batteries

type t = {
    lsb_cache: LocalSizeBound.lsb_cache;
    rvg_cache: RVGTypes.RVG.rvg_cache;
    elcb_cache: ExpLocalChangeBound.elcb_cache;
    pre_cache: Program.pre_cache;
    trans_id_counter: TransitionLabel.trans_id_counter;
    ranking_cache: RankingFunction.ranking_cache;
    metering_cache: MeteringRSM.metering_cache;
    lrsm_cache: LexRSM.lrsm_cache
}

let lsb_cache t = t.lsb_cache

let rvg_cache t = t.rvg_cache

let elcb_cache t = t.elcb_cache

let pre_cache t = t.pre_cache

let trans_id_counter t = t.trans_id_counter

let ranking_cache t = t.ranking_cache

let metring_cache t = t.metering_cache

let lrsm_cache t = t.lrsm_cache

let new_cache_with_counter trans_id_counter = fun () ->
    {
      lsb_cache = LocalSizeBound.new_cache ();
      rvg_cache = RVGTypes.RVG.new_cache ();
      elcb_cache = ExpLocalChangeBound.new_cache ();
      pre_cache = Program.new_cache ();
      trans_id_counter;
      ranking_cache = RankingFunction.new_cache ();
      metering_cache = MeteringRSM.new_cache ();
      lrsm_cache = LexRSM.new_cache ();
    }

let new_cache = fun () -> new_cache_with_counter (TransitionLabel.new_trans_id_counter ()) ()

let clear_lrsm_cache cache = fun () -> {cache with lrsm_cache = LexRSM.new_cache ()}
