
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>

#define TRY(x) {if(( x ) != 0) {fprintf(stderr, "*** error in "#x"\n"); exit(-1);}}


// This little program is *almost* able to take some arbitrary encoded
// audio file and output some sensible output sample-format (typically
// s16 or flp). It works, but sometimes a stereo becomes mono (if
// output sample format is planar).

int main(int argc, char* argv[]) {
  int ret = 0;
  AVFormatContext *fmt_ctx = 0;
  AVCodec *dec;
  char* filename = argv[1];

  av_register_all();

  if(argc <= 1)  {
    fprintf(stderr, "usage: <filename>\n");
    exit(-1);
  }

  fprintf(stderr, "decoding %s ... \n", filename);
  TRY(avformat_open_input(&fmt_ctx, filename, NULL, NULL));
  TRY(avformat_find_stream_info(fmt_ctx, 0));
  /* av_dump_format(fmt_ctx, 0, "some file", 0); */

  int stream_idx = av_find_best_stream(fmt_ctx, AVMEDIA_TYPE_AUDIO, -1, -1, &dec, 0);
  if(stream_idx < 0) {
    fprintf(stderr, "fail best stream\n");
    exit(-1);
  }

  AVPacket pkt;
  pkt.buf = 0;
  AVCodecContext *dec_ctx = fmt_ctx->streams[stream_idx]->codec;

  if(ret = avcodec_open2(dec_ctx, dec, NULL) < 0) {
    fprintf(stderr, "error with avcodec_open: %s\n", av_err2str(ret));
    exit(-1);
  }

  fprintf(stderr, "input codec %s (%s)\n", dec->long_name, dec->name);
  fprintf(stderr, "output sample format: %s\n", av_get_sample_fmt_name(dec_ctx->sample_fmt));
  fprintf(stderr, "output sample rate:   %d\n", (dec_ctx->sample_rate));

  AVFrame *frame = av_frame_alloc();
  int got_frame;
  int len1 = 0;

  // process packets:
  while(av_read_frame(fmt_ctx, &pkt) == 0) {
    // fprintf(stderr, "read frame, size = \t %d,\t index = %d\n", pkt.size, pkt.stream_index);
    AVPacket original_pkt = pkt;
    // process frames:
    do {
      got_frame = 0;
      len1 = avcodec_decode_audio4(dec_ctx, frame, &got_frame, &pkt);
      if(len1 < 0) {
        fprintf(stderr, "error while decoding: %s\n", av_err2str(len1));
        exit(-1);
      }
      if(got_frame) {
        size_t unpadded_linesize = frame->nb_samples * av_get_bytes_per_sample(frame->format);
        fwrite(frame->extended_data[0], 1, unpadded_linesize, stdout);
      }

      pkt.data += len1;
      pkt.size -= len1;
    } while (pkt.size > 0);

    av_free_packet(&original_pkt);
  }

  avformat_close_input(&fmt_ctx);

}
