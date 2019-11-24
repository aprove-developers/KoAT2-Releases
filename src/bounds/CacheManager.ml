open Batteries

type t = {
    lsb_cache: LocalSizeBound.lsb_cache;
    elsb_cache: ExpLocalSizeBound.elsb_cache;
    trans_id_counter: TransitionLabel.trans_id_counter;
    ranking_cache: RankingFunction.ranking_cache;
    metering_cache: MeteringRSM.metering_cache;
    lrsm_cache: LexRSM.lrsm_cache
}

let lsb_cache t = t.lsb_cache

let elsb_cache t = t.elsb_cache

let trans_id_counter t = t.trans_id_counter

let ranking_cache t = t.ranking_cache

let metring_cache t = t.metering_cache

let lrsm_cache t = t.lrsm_cache

let new_cache = fun () ->
    {
      lsb_cache = LocalSizeBound.new_cache ();
      elsb_cache = ExpLocalSizeBound.new_cache ();
      trans_id_counter = TransitionLabel.new_trans_id_counter ();
      ranking_cache = RankingFunction.new_cache ();
      metering_cache = MeteringRSM.new_cache ();
      lrsm_cache = LexRSM.new_cache ();
    }