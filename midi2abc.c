/*
 * midi2abc - program to convert MIDI files to abc notation.
 * Copyright (C) 1998 James Allwright
 * e-mail: J.R.Allwright@westminster.ac.uk
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

/* new midi2abc - converts MIDI file to abc format files
 * 
 *
 * re-written to use dynamic data structures 
 *              James Allwright
 *               5th June 1998
 *
 * added output file option -o
 * added summary option -sum
 * added option -u to enter xunit directly
 * fixed computation of xunit using -b option
 * added -obpl (one bar per line) option
 * add check for carriage return embedded inside midi text line
 *                Seymour Shlien  04/March/00
 * made to conform as much as possible to the official version.
 * check for drum track added
 * when midi program channel is command encountered, we ensure that 
 * we are using the correct channel number for the Voice by sending
 * a %%MIDI channel message.
 *                Seymour Shlien  9/December/00
 * 
 * based on public domain 'midifilelib' package.
 *
 */

#define VERSION "2.77 December 17 2004"

/* Microsoft Visual C++ Version 6.0 or higher */
#ifdef _MSC_VER
#define ANSILIBS
#endif

#include <stdio.h>
#ifdef PCCFIX
#define stdout 1
#endif

/* define USE_INDEX if your C libraries have index() instead of strchr() */
#ifdef USE_INDEX
#define strchr index
#endif

#ifdef ANSILIBS
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#else
extern char* malloc();
extern char* strchr();
#endif
#include "midifile.h"
#define BUFFSIZE 200
/* declare MIDDLE C */
#define MIDDLE 72
void initfuncs();
void setupkey(int);
int testtrack(int trackno, int barbeats, int anacrusis);
int open_note(int chan, int pitch, int vol);
int close_note(int chan, int pitch, int *initvol);


/* Global variables and structures */

static FILE *F;
static FILE *outhandle; /* for producing the abc file */

int division;    /* pulses per quarter note defined in MIDI header    */
long tempo = 500000; /* the default tempo is 120 quarter notes/minute */
int unitlen;     /* abc unit length usually defined in L: field       */
int header_unitlen; /* first unitlen set                              */
int unitlen_set =0; /* once unitlen is set don't allow it to change   */
int parts_per_unitlen = 2; /* specifies minimum quantization size */
long laston = 0; /* length of MIDI track in pulses or ticks           */
char textbuff[BUFFSIZE]; /*buffer for handling text output to abc file*/
int trans[256], back[256]; /*translation tables for MIDI pitch to abc note*/
char atog[256]; /* translation tables for MIDI pitch to abc note */
int symbol[256]; /*translation tables for MIDI pitch to abc note */
int key[12];
int sharps;
int trackno, maintrack;
int format; /* MIDI file type                                   */

int karaoke, inkaraoke;
int midline;
int tempocount=0;  /* number of tempo indications in MIDI file */
int gotkeysig=0; /*set to 1 if keysignature found in MIDI file */

/* global parameters that may be set by command line options    */
int xunit;    /* pulses per abc unit length                     */
int tsig_set; /* flag - time signature already set by user      */
int ksig_set; /* flag - key signature already set by user       */
int xunit_set;/* flat - xunit already set by user               */
int extracta; /* flag - get anacrusis from strong beat          */
int guessu;   /* flag - estimate xunit from note durations      */
int guessa;   /* flag - get anacrusis by minimizing tied notes  */
int guessk;   /* flag - guess key signature                     */
int summary;  /* flag - output summary info of MIDI file        */
int keep_short; /*flag - preserve short notes                   */
int swallow_rests; /* flag - absorb short rests                 */
int midiprint; /* flag - run midigram instead of midi2abc       */
int restsize; /* smallest rest to absorb                        */
int no_triplets; /* flag - suppress triplets or broken rhythm   */
int obpl = 0; /* flag to specify one bar per abc text line      */
int bars_per_line=4;  /* number of bars per output line         */
int bars_per_staff=4; /* number of bars per music staff         */
int asig, bsig;  /* time signature asig/bsig                    */
int header_asig =0; /* first time signature encountered         */
int header_bsig =0; /* first time signature encountered         */
int header_bb;      /* first ticks/quarter note encountered     */
int active_asig,active_bsig;  /* last time signature declared   */
int last_asig, last_ksig; /* last time signature printed        */
int barsize; /* barsize in parts_per_unitlen units                   */
int Qval;        /* tempo - quarter notes per minute            */
int verbosity=0; /* control amount of detail messages in abcfile*/

/* global arguments dependent on command line options or computed */

int anacrusis=0; 
int bars;
int keysig;
int header_keysig=  -50;  /* header key signature                     */
int active_keysig = -50;  /* last key signature declared        */
int xchannel;  /* channel number to be extracted. -1 means all  */


/* structure for storing music notes */
struct anote {
  int pitch;  /* MIDI pitch    */
  int chan;   /* MIDI channel  */
  int vel;    /* MIDI velocity */
  long time;  /* MIDI onset time in pulses */
  long dtnext; /* time increment to next note in pulses */
  long tplay;  /* note duration in pulses */
  int xnum;    /* note duration in number of xunits */
  int playnum;
  int denom;
};


/* linked list of notes */
struct listx {
  struct listx* next;
  struct anote* note;
};

/* linked list of text items (strings) */
struct tlistx {
  struct tlistx* next;
  char* text;
  long when; 	/* time in pulses to output */
  int type;     /* 0 - comments, other - field commands */
};


/* a MIDI track */
struct atrack {
  struct listx* head;    /* first note */
  struct listx* tail;    /* last note */
  struct tlistx* texthead; /* first text string */
  struct tlistx* texttail; /* last text string */
  int notes;             /* number of notes in track */
  long tracklen;
  long startwait;
  int startunits;
  int drumtrack;
};

/* can cope with up to 64 track MIDI files */
struct atrack track[64];
int trackcount = 0;
int maxbarcount = 0;
/* maxbarcount  is used to return the numbers of bars created.
/* obpl is a flag for one bar per line. */

/* double linked list of notes */
/* used for temporary list of chords while abc is being generated */
struct dlistx {
  struct dlistx* next;
  struct dlistx* last;
  struct anote* note;
};

int notechan[2048],notechanvol[2048]; /*for linking on and off midi
					channel commands            */
int last_tick; /* for getting last pulse number in MIDI file */



void remove_carriage_returns();
int validnote();





/*              Stage 1. Parsing MIDI file                   */

/* Functions called during the reading pass of the MIDI file */

/* The following C routines are required by midifilelib.  */
/* They specify the action to be taken when various items */
/* are encountered in the MIDI.  The mfread function scans*/
/* the MIDI file and calls these functions when needed.   */


int filegetc()
{
    return(getc(F));
}


void fatal_error(s)
char* s;
/* fatal error encounterd - abort program */
{
  fprintf(stderr, "%s\n", s);
  exit(1);
}


void event_error(s)
char *s;
/* problem encountered but OK to continue */
{
  char msg[256];

  sprintf(msg, "Error: Time=%ld Track=%d %s\n", Mf_currtime, trackno, s);
  printf(msg);
}


int* checkmalloc(bytes)
/* malloc with error checking */
int bytes;
{
  int *p;

  p = (int*) malloc(bytes);
  if (p == NULL) {
    fatal_error("Out of memory error - cannot malloc!");
  };
  return (p);
}



char* addstring(s)
/* create space for string and store it in memory */
char* s;
{
  char* p;

  p = (char*) checkmalloc(strlen(s)+1);
  strcpy(p, s);
  return(p);
}

void addtext(s, type)
/* add structure for text */
/* used when parsing MIDI file */
char* s;
int type;
{
  struct tlistx* newx;

  newx = (struct tlistx*) checkmalloc(sizeof(struct tlistx));
  newx->next = NULL;
  newx->text = addstring(s);
  newx->type = type;
  newx->when = Mf_currtime;
  if (track[trackno].texthead == NULL) {
    track[trackno].texthead = newx;
    track[trackno].texttail = newx;
  }
  else {
    track[trackno].texttail->next = newx;
    track[trackno].texttail = newx;
  };
}
  


/* The MIDI file has  separate commands for starting                 */
/* and stopping a note. In order to determine the duration of        */
/* the note it is necessary to find the note_on command associated   */
/* with the note off command. We rely on the note's pitch and channel*/
/* number to find the right note. While we are parsing the MIDI file */
/* we maintain a list of all the notes that are currently on         */
/* head and tail of list of notes still playing.                     */
/* The following doubly linked list is used for this purpose         */

struct dlistx* playinghead;
struct dlistx* playingtail; 


void noteplaying(p)
/* This function adds a new note to the playinghead list. */
struct anote* p;
{
  struct dlistx* newx;

  newx = (struct dlistx*) checkmalloc(sizeof(struct dlistx));
  newx->note = p;
  newx->next = NULL;
  newx->last = playingtail;
  if (playinghead == NULL) {
    playinghead = newx;
  };
  if (playingtail == NULL) {
    playingtail = newx;
  } 
  else {
    playingtail->next = newx;
    playingtail = newx;
  };
}


void addnote(p, ch, v)
/* add structure for note */
/* used when parsing MIDI file */
int p, v;
{
  struct listx* newx;
  struct anote* newnote;

  track[trackno].notes = track[trackno].notes + 1;
  newx = (struct listx*) checkmalloc(sizeof(struct listx));
  newnote = (struct anote*) checkmalloc(sizeof(struct anote));
  newx->next = NULL;
  newx->note = newnote;
  if (track[trackno].head == NULL) {
    track[trackno].head = newx;
    track[trackno].tail = newx;
  } 
  else {
    track[trackno].tail->next = newx;
    track[trackno].tail = newx;
  };
  if (ch == 9) {
    track[trackno].drumtrack = 1;
  };
  newnote->pitch = p;
  newnote->chan = ch;
  newnote->vel = v;
  newnote->time = Mf_currtime;
  laston = Mf_currtime;
  newnote->tplay = Mf_currtime;
  noteplaying(newnote);
}



void notestop(p, ch)
/* MIDI note stops */
/* used when parsing MIDI file */
int p, ch;
{
  struct dlistx* i;
  int found;
  char msg[80];

  i = playinghead;
  found = 0;
  while ((found == 0) && (i != NULL)) {
    if ((i->note->pitch == p)&&(i->note->chan==ch)) {
      found = 1;
    } 
    else {
      i = i->next;
    };
  };
  if (found == 0) {
    sprintf(msg, "Note terminated when not on - pitch %d", p);
    event_error(msg);
    return;
  };
  /* fill in tplay field */
  i->note->tplay = Mf_currtime - (i->note->tplay);
  /* remove note from list */
  if (i->last == NULL) {
    playinghead = i->next;
  } 
  else {
    (i->last)->next = i->next;
  };
  if (i->next == NULL) {
    playingtail = i->last;
  } 
  else {
    (i->next)->last = i->last;
  };
  free(i);
}




FILE *
efopen(name,mode)
char *name;
char *mode;
{
    FILE *f;

    if ( (f=fopen(name,mode)) == NULL ) {
      char msg[256];
      sprintf(msg,"Error - Cannot open file %s",name);
      fatal_error(msg);
    }
    return(f);
}


void error(s)
char *s;
{
    fprintf(stderr,"Error: %s\n",s);
}



void txt_header(xformat,ntrks,ldivision)
int xformat, ntrks, ldivision;
{
    division = ldivision; 
    format = xformat;
    if (format != 0) {
    /*  fprintf(outhandle,"%% format %d file %d tracks\n", format, ntrks);*/
      if(summary>0) printf("This midi file has %d tracks\n\n",ntrks);
    } 
    else {
/*     fprintf(outhandle,"%% type 0 midi file\n"); */
     if(summary>0) {
	     printf("This is a type 0 midi file.\n");
             printf("All the channels are in one track.\n");
             printf("You may need to process the channels separately\n\n");
            }
     }
     
}


void txt_trackstart()
{
  laston = 0L;
  track[trackno].notes = 0;
  track[trackno].head = NULL;
  track[trackno].tail = NULL;
  track[trackno].texthead = NULL;
  track[trackno].texttail = NULL;
  track[trackno].tracklen = Mf_currtime;
  track[trackno].drumtrack = 0;
}

void txt_trackend()
{
  /* check for unfinished notes */
  if (playinghead != NULL) {
    printf("Error in MIDI file - notes still on at end of track!\n");
  };
  track[trackno].tracklen = Mf_currtime - track[trackno].tracklen;
  trackno = trackno + 1;
  trackcount = trackcount + 1;
}

void txt_noteon(chan,pitch,vol)
int chan, pitch, vol;
{
  if ((xchannel == -1) || (chan == xchannel)) {
    if (vol != 0) {
      addnote(pitch, chan, vol);
    } 
    else {
      notestop(pitch, chan);
    };
  };
}

void txt_noteoff(chan,pitch,vol)
int chan, pitch, vol;
{
  if ((xchannel == -1) || (chan == xchannel)) {
    notestop(pitch, chan);
  };
}

void txt_pressure(chan,pitch,press)
int chan, pitch, press;
{
}

void txt_parameter(chan,control,value)
int chan, control, value;
{
}

void txt_pitchbend(chan,msb,lsb)
int chan, msb, lsb;
{
}

void txt_program(chan,program)
int chan, program;
{
/*
  sprintf(textbuff, "%%%%MIDI program %d %d",
         chan+1, program);
*/
  sprintf(textbuff, "%%%%MIDI program %d", program);
  addtext(textbuff,0);
/* abc2midi does not use the same channel number as specified in 
  the original midi file, so we should not specify that channel
  number in the %%MIDI program. If we leave it out the program
  will refer to the current channel assigned to this voice.
*/
}

void txt_chanpressure(chan,press)
int chan, press;
{
}

void txt_sysex(leng,mess)
int leng;
char *mess;
{
}

void txt_metamisc(type,leng,mess)
int type, leng;
char *mess;
{
}

void txt_metaspecial(type,leng,mess)
int type, leng;
char *mess;
{
}

void txt_metatext(type,leng,mess)
int type, leng;
char *mess;
{ 
    char *ttype[] = {
    NULL,
    "Text Event",        /* type=0x01 */
    "Copyright Notice",    /* type=0x02 */
    "Sequence/Track Name",
    "Instrument Name",    /* ...     */
    "Lyric",
    "Marker",
    "Cue Point",        /* type=0x07 */
    "Unrecognized"
  };
  int unrecognized = (sizeof(ttype)/sizeof(char *)) - 1;
  unsigned char c;
  int n;
  char *p = mess;
  char *buff;
  char buffer2[BUFFSIZE];

  if ((type < 1)||(type > unrecognized))
      type = unrecognized;
  buff = textbuff;
  for (n=0; n<leng; n++) {
    c = *p++;
    if (buff - textbuff < BUFFSIZE - 6) {
      sprintf(buff, 
           (isprint(c)||isspace(c)) ? "%c" : "\\0x%02x" , c);
      buff = buff + strlen(buff);
    };
  }
  if (strncmp(textbuff, "@KMIDI KARAOKE FILE", 14) == 0) {
    karaoke = 1;
  } 
  else {
    if ((karaoke == 1) && (*textbuff != '@')) {
      addtext(textbuff,0);
    } 
    else {
      if (leng < BUFFSIZE - 3) {
        sprintf(buffer2, " %s", textbuff); 
        addtext(buffer2,0);
      };
    };
  };
}

void txt_metaseq(num)
int num;
{  
  sprintf(textbuff, "%%Meta event, sequence number = %d",num);
  addtext(textbuff,0);
}

void txt_metaeot()
/* Meta event, end of track */
{
}

void txt_keysig(sf,mi)
char sf, mi;
{
  int accidentals;
  gotkeysig =1;
  sprintf(textbuff, 
         "%% MIDI Key signature, sharp/flats=%d  minor=%d",
          (int) sf, (int) mi);
  if(verbosity) addtext(textbuff,0);
  sprintf(textbuff,"%d %d\n",sf,mi);
  if (!ksig_set) {
	  addtext(textbuff,1);
	  keysig=sf;
  }
  if (header_keysig == -50) header_keysig = keysig;
  if (summary <= 0) return;
  /* There may be several key signature changes in the midi
     file so that key signature in the mid file does not conform
     with the abc file. Show all key signature changes. 
  */   
  accidentals = (int) sf;
  if (accidentals <0 )
    {
    accidentals = -accidentals;
    printf("Key signature: %d flats", accidentals);
    }
  else
     printf("Key signature : %d sharps", accidentals);
  if (ksig_set) printf(" suppressed\n");
  else printf("\n");
}

void txt_tempo(ltempo)
long ltempo;
{
    if(tempocount>0) return; /* ignore other tempo indications */
    tempo = ltempo;
    tempocount++;
}


void setup_timesig(nn,  denom,  bb)
int nn,denom,bb;
{
  asig = nn;
  bsig = denom;
/* we must keep unitlen and xunit fixed for the entire tune */
  if (unitlen_set == 0) {
    unitlen_set = 1; 
    if ((asig*4)/bsig >= 3) {
      unitlen =8;
      } 
    else {
      unitlen = 16;
      };
   }
/* set xunit for this unitlen */
  if(!xunit_set) xunit = (division*bb*4)/(8*unitlen);
  barsize = parts_per_unitlen*asig*unitlen/bsig;
/*  printf("setup_timesig: unitlen=%d xunit=%d barsize=%d\n",unitlen,xunit,barsize); */
  if (header_asig ==0) {header_asig = asig;
	                header_bsig = bsig;
			header_unitlen = unitlen;
			header_bb = bb;
                       }
}


void txt_timesig(nn,dd,cc,bb)
int nn, dd, cc, bb;
{
  int denom = 1;
  while ( dd-- > 0 )
    denom *= 2;
  sprintf(textbuff, 
          "%% Time signature=%d/%d  MIDI-clocks/click=%d  32nd-notes/24-MIDI-clocks=%d", 
    nn,denom,cc,bb);
  if (verbosity) addtext(textbuff,0);
  sprintf(textbuff,"%d %d %d\n",nn,denom,bb);
  if (!tsig_set) {
	  addtext(textbuff,2);
          setup_timesig(nn, denom,bb);
   }
  if (summary>0) {
    if(tsig_set) printf("Time signature = %d/%d suppressed\n",nn,denom);
    else printf("Time signature = %d/%d\n",nn,denom);
    }
}


void txt_smpte(hr,mn,se,fr,ff)
int hr, mn, se, fr, ff;
{
}

void txt_arbitrary(leng,mess)
char *mess;
int leng;
{
}



/* Dummy functions for handling MIDI messages.
 *    */
 void no_op0() {}
 void no_op1(int dummy1) {}
 void no_op2(int dummy1, int dummy2) {}
 void no_op3(int dummy1, int dummy2, int dummy3) { }
 void no_op4(int dummy1, int dummy2, int dummy3, int dummy4) { }
 void no_op5(int dummy1, int dummy2, int dummy3, int dummy4, int dummy5) { }



void print_txt_noteon(chan, pitch, vol)
{
int start_time;
int initvol;
if (vol > 0)
open_note(chan, pitch, vol);
else {
  start_time = close_note(chan, pitch,&initvol);
  if (start_time >= 0)
      printf("%8.4f %8.4f %d %d %d %d\n",
       (double) start_time/(double) division,
       (double) Mf_currtime/(double) division,
       trackno+1, chan +1, pitch,initvol);
      if(Mf_currtime > last_tick) last_tick = Mf_currtime;
   }
}



void print_txt_noteoff(chan, pitch, vol)
{
int start_time,initvol;

start_time = close_note(chan, pitch, &initvol);
if (start_time >= 0)
    printf("%8.4f %8.4f %d %d %d %d\n",
     (double) start_time/(double) division,
     (double) Mf_currtime/(double) division,
     trackno+1, chan+1, pitch,initvol);
    if(Mf_currtime > last_tick) last_tick = Mf_currtime;
}



/* In order to associate a channel note off message with its
 * corresponding note on message, we maintain the information
 * the notechan array. When a midi pitch (0-127) is switched
 * on for a particular channel, we record the time that it
 * was turned on in the notechan array. As there are 16 channels
 * and 128 pitches, we initialize an array 128*16 = 2048 elements
 * long.
**/
init_notechan()
{
/* signal that there are no active notes */
 int i;
 for (i = 0; i < 2048; i++) notechan[i] = -1;
}


/* The next two functions update notechan when a channel note on
   or note off is encountered. The second function close_note,
   returns the time when the note was turned on.
*/
int open_note(int chan, int pitch, int vol)
{
    notechan[128 * chan + pitch] = Mf_currtime;
    notechanvol[128 * chan + pitch] = vol;
    return 0;
}


int close_note(int chan, int pitch, int *initvol)
{
    int index, start_tick;
    index = 128 * chan + pitch;
    if (notechan[index] < 0)
	return -1;
    start_tick = notechan[index];
    *initvol = notechanvol[index];
    notechan[index] = -1;
    return start_tick;
}


void initfunc_for_midinotes()
{
    Mf_error = error;
    Mf_header = txt_header;
    Mf_trackstart = txt_trackstart;
    Mf_trackend = txt_trackend;
    Mf_noteon = print_txt_noteon;
    Mf_noteoff = print_txt_noteoff;
    Mf_pressure = no_op3;
    Mf_parameter = no_op3;
    Mf_pitchbend = no_op3;
    Mf_program = no_op2;
    Mf_chanpressure = no_op3;
    Mf_sysex = no_op2;
    Mf_metamisc = no_op3;
    Mf_seqnum = no_op1;
    Mf_eot = no_op0;
    Mf_timesig = no_op4;
    Mf_smpte = no_op5;
    Mf_tempo = no_op1;
    Mf_keysig = no_op2;
    Mf_seqspecific = no_op3;
    Mf_text = no_op3;
    Mf_arbitrary = no_op2;
}




void initfuncs()
{
    Mf_error = error;
    Mf_header =  txt_header;
    Mf_trackstart =  txt_trackstart;
    Mf_trackend =  txt_trackend;
    Mf_noteon =  txt_noteon;
    Mf_noteoff =  txt_noteoff;
    Mf_pressure =  txt_pressure;
    Mf_parameter =  txt_parameter;
    Mf_pitchbend =  txt_pitchbend;
    Mf_program =  txt_program;
    Mf_chanpressure =  txt_chanpressure;
    Mf_sysex =  txt_sysex;
    Mf_metamisc =  txt_metamisc;
    Mf_seqnum =  txt_metaseq;
    Mf_eot =  txt_metaeot;
    Mf_timesig =  txt_timesig;
    Mf_smpte =  txt_smpte;
    Mf_tempo =  txt_tempo;
    Mf_keysig =  txt_keysig;
    Mf_seqspecific =  txt_metaspecial;
    Mf_text =  txt_metatext;
    Mf_arbitrary =  txt_arbitrary;
}


/*  Stage 2 Quantize MIDI tracks. Get key signature, time signature...   */ 


void postprocess(trackno)
/* This routine calculates the time interval before the next note */
/* called after the MIDI file has been read in */
int trackno;
{
  struct listx* i;

  i = track[trackno].head;
  if (i != NULL) {
    track[trackno].startwait = i->note->time;
  } 
  else {
    track[trackno].startwait = 0;
  };
  while (i != NULL) {
    if (i->next != NULL) {
      i->note->dtnext = i->next->note->time - i->note->time;
    } 
    else {
      i->note->dtnext = i->note->tplay;
    };
    i = i->next;
  };
}

void scannotes(trackno)
int trackno;
/* diagnostic routine to output notes in a track */
{
  struct listx* i;

  i = track[trackno].head;
  while (i != NULL) {
    printf("Pitch %d chan %d vel %d time %ld xnum %d playnum %d\n",
            i->note->pitch, i->note->chan, 
            i->note->vel, i->note->dtnext,
            i->note->xnum, i->note->playnum);
    i = i->next;
  };
}



int quantize(trackno, xunit)
/* Work out how long each note is in musical time units.
 * The results are placed in note.playnum              */
int trackno, xunit;
{
  struct listx* j;
  struct anote* this;
  int spare;
  int toterror;
  int quantum;

  /* fix to avoid division by zero errors in strange MIDI */
  if (xunit == 0) {
    return(10000);
  };
  quantum = 2.*xunit/parts_per_unitlen; /* xunit assume 2 parts_per_unit */
  track[trackno].startunits = (2*(track[trackno].startwait + (quantum/4)))/quantum;
  spare = 0;
  toterror = 0;
  j = track[trackno].head;
  while (j != NULL) {
    this = j->note;
    /* this->xnum is the quantized inter onset time */
    /* this->playnum is the quantized note length   */
    this->xnum = (2*(this->dtnext + spare + (quantum/4)))/quantum;
    this->playnum = (2*(this->tplay + (quantum/4)))/quantum;
    if ((this->playnum == 0) && (keep_short)) {
      this->playnum = 1;
    };
    /* In the event of short rests, the inter onset time
     * will be larger than the note length. However, for
     * chords the inter onset time can be zero.          */
    if ((swallow_rests>=0) && (this->xnum - this->playnum <= restsize)
		        && this->xnum > 0) {
      this->playnum = this->xnum;
    };
    this->denom = parts_per_unitlen; /* this variable is never used ! */
    spare = spare + this->dtnext - (this->xnum*xunit/this->denom);
    if (spare > 0) {
      toterror = toterror + spare;
    } 
    else {
      toterror = toterror - spare;
    };
    /* gradually forget old errors so that if xunit is slightly off,
       errors don't accumulate over several bars */
    spare = (spare * 96)/100;
    j = j->next;
  };
  return(toterror);
}


void guesslengths(trackno)
/* work out most appropriate value for a unit of musical time */
int trackno;
{
  int i;
  int trial[100];
  float avlen, factor, tryx;
  long min;

  min = track[trackno].tracklen;
  if (track[trackno].notes == 0) {
    return;
  };
  avlen = ((float)(min))/((float)(track[trackno].notes));
  tryx = avlen * 0.75;
  factor = tryx/100;
  for (i=0; i<100; i++) {
    trial[i] = quantize(trackno, (int) tryx);
    if ((long) trial[i] < min) {
      min = (long) trial[i];
      xunit = (int) tryx;
    };
    tryx = tryx + factor;
  };
}


int findana(maintrack, barsize)
/* work out anacrusis from MIDI */
/* look for a strong beat marking the start of a bar */
int maintrack;
int barsize;
{
  int min, mincount;
  int place;
  struct listx* p;

  min = 0;
  mincount = 0;
  place = 0;
  p = track[maintrack].head;
  while ((p != NULL) && (place < barsize)) {
    if ((p->note->vel > min) && (place > 0)) {
      min = p->note->vel;
      mincount = place;
    };
    place = place + (p->note->xnum);
    p = p->next;
  };
  return(mincount);
}



int guessana(barbeats)
int barbeats;
/* try to guess length of anacrusis */
{
  int score[64];
  int min, minplace;
  int i,j;

  if (barbeats > 64) {
    fatal_error("Bar size exceeds static limit of 64 units!");
  };
  for (j=0; j<barbeats; j++) {
    score[j] = 0;
    for (i=0; i<trackcount; i++) {
      score[j] = score[j] + testtrack(i, barbeats, j);
      /* restore values to num */
      quantize(i, xunit);
    };
  };
  min = score[0];
  minplace = 0;
  for (i=0; i<barbeats; i++) {
    if (score[i] < min) {
      min = score[i];
      minplace = i;
    };
  };
  return(minplace);
}


int findkey(maintrack)
int maintrack;
/* work out what key MIDI file is in */
/* algorithm is simply to minimize the number of accidentals needed. */
{
  int j;
  int max, min, n[12], key_score[12];
  int minkey, minblacks;
  static int keysharps[12] = {
	  0, -5, 2, -3, 4, -1, 6, 1, -4, 3, -2, 5};
  struct listx* p;
  int thispitch;
  int lastpitch;
  int totalnotes;

  /* analyse pitches */
  /* find key */
  for (j=0; j<12; j++) {
    n[j] = 0;
  };
  min = track[maintrack].tail->note->pitch;
  max = min;
  totalnotes = 0;
  for (j=0; j<trackcount; j++) {
    totalnotes = totalnotes + track[j].notes;
    p = track[j].head;
    while (p != NULL) {
      thispitch = p->note->pitch;
      if (thispitch > max) {
        max = thispitch;
      } 
      else {
        if (thispitch < min) {
          min = thispitch;
        };
      };
      n[thispitch % 12] = n[thispitch % 12] + 1;
      p = p->next;
    };
  };
  /* count black notes for each key */
  /* assume pitch = 0 is C */
  minkey = 0;
  minblacks = totalnotes;
  for (j=0; j<12; j++) {
    key_score[j] = n[(j+1)%12] + n[(j+3)%12] + n[(j+6)%12] +
                   n[(j+8)%12] + n[(j+10)%12];
    /* printf("Score for key %d is %d\n", j, key_score[j]); */
    if (key_score[j] < minblacks) {
      minkey = j;
      minblacks = key_score[j];
    };
  };
  /* do conversion to abc pitches */
  /* Code changed to use absolute rather than */
  /* relative choice of pitch for 'c' */
  /* MIDDLE = (min + (max - min)/2 + 6)/12 * 12; */
  /* Do last note analysis */
  lastpitch = track[maintrack].tail->note->pitch;
  if (minkey != (lastpitch%12)) {
    fprintf(outhandle,"%% Last note suggests ");
    switch((lastpitch+12-minkey)%12) {
    case(2):
      fprintf(outhandle,"Dorian ");
      break;
    case(4):
      fprintf(outhandle,"Phrygian ");
      break;
    case(5):
      fprintf(outhandle,"Lydian ");
      break;
    case(7):
      fprintf(outhandle,"Mixolydian ");
      break;
    case(9):
      fprintf(outhandle,"minor ");
      break;
    case(11):
      fprintf(outhandle,"Locrian ");
      break;
    default:
      fprintf(outhandle,"unknown ");
      break;
    };
    fprintf(outhandle,"mode tune\n");
  };
  /* switch to minor mode if it gives same number of accidentals */
  if ((minkey != ((lastpitch+3)%12)) && 
      (key_score[minkey] == key_score[(lastpitch+3)%12])) {
         minkey = (lastpitch+3)%12;
  };
  /* switch to major mode if it gives same number of accidentals */
  if ((minkey != (lastpitch%12)) && 
      (key_score[minkey] == key_score[lastpitch%12])) {
         minkey = lastpitch%12;
  };
  sharps = keysharps[minkey];
  return(sharps);
}




/* Stage 3  output MIDI tracks in abc format                        */


/* head and tail of list of notes in current chord playing */
/* used while abc is being generated */
struct dlistx* chordhead;
struct dlistx* chordtail;


void printchordlist()
/* diagnostic routine */
{
  struct dlistx* i;

  i = chordhead;
  printf("----CHORD LIST------\n");
  while(i != NULL) {
    printf("pitch %d len %d\n", i->note->pitch, i->note->playnum);
    if (i->next == i) {
      fatal_error("Loopback problem!");
    };
    i = i->next;
  };
}

void checkchordlist()
/* diagnostic routine */
/* validates data structure */
{
  struct dlistx* i;
  int n;

  if ((chordhead == NULL) && (chordtail == NULL)) {
    return;
  };
  if ((chordhead == NULL) && (chordtail != NULL)) {
    fatal_error("chordhead == NULL and chordtail != NULL");
  };
  if ((chordhead != NULL) && (chordtail == NULL)) {
    fatal_error("chordhead != NULL and chordtail == NULL");
  };
  if (chordhead->last != NULL) {
    fatal_error("chordhead->last != NULL");
  };
  if (chordtail->next != NULL) {
    fatal_error("chordtail->next != NULL");
  };
  i = chordhead;
  n = 0;
  while((i != NULL) && (i->next != NULL)) {
    if (i->next->last != i) {
      char msg[80];

      sprintf(msg, "chordlist item %d : i->next->last!", n);
      fatal_error(msg);
    };
    i = i->next;
    n = n + 1;
  };
  /* checkchordlist(); */
}

void addtochord(p)
/* used when printing out abc */
struct anote* p;
{
  struct dlistx* newx;
  struct dlistx* place;

  newx = (struct dlistx*) checkmalloc(sizeof(struct dlistx));
  newx->note = p;
  newx->next = NULL;
  newx->last = NULL;

  if (chordhead == NULL) {
    chordhead = newx;
    chordtail = newx;
    checkchordlist();
    return;
  };
  place = chordhead;
  while ((place != NULL) && (place->note->pitch > p->pitch)) {
    place = place->next;
  };
  if (place == chordhead) {
    newx->next = chordhead;
    chordhead->last = newx;
    chordhead = newx;
    checkchordlist();
    return;
  };
  if (place == NULL) {
    newx->last = chordtail;
    chordtail->next = newx;
    chordtail = newx;
    checkchordlist();
    return;
  };
  newx->next = place;
  newx->last = place->last;
  place->last = newx;
  newx->last->next = newx;
  checkchordlist();
}

struct dlistx* removefromchord(i)
/* used when printing out abc */
struct dlistx* i;
{
  struct dlistx* newi;

  /* remove note from list */
  if (i->last == NULL) {
    chordhead = i->next;
  } 
  else {
    (i->last)->next = i->next;
  };
  if (i->next == NULL) {
    chordtail = i->last;
  } 
  else {
    (i->next)->last = i->last;
  };
  newi = i->next;
  free(i);
  checkchordlist();
  return(newi);
}

int findshortest(gap)
/* find the first note in the chord to terminate */
int gap;
{
  int min, v;
  struct dlistx* p;

  p = chordhead;
  min = gap;
  while (p != NULL) {
    v = p->note->playnum;
    if (v < min) {
      min = v;
    };
    p = p->next;
  };
  return(min);
}

void advancechord(len)
/* adjust note lengths for all notes in the chord */
int len;
{
  struct dlistx* p;

  p = chordhead;
  while (p != NULL) {
    if (p->note->playnum <= len) {
      if (p->note->playnum < len) {
        fatal_error("Error - note too short!");
      };
      /* remove note */
      checkchordlist();
      p = removefromchord(p);
    } 
    else {
      /* shorten note */
      p->note->playnum = p->note->playnum - len;
      p = p->next;
    };
  };
}

void freshline()
/* if the current line of abc or text is non-empty, start a new line */
{
  if (midline == 1) {
    fprintf(outhandle,"\n");
    midline = 0;
  };
}

int testtrack(trackno, barbeats, anacrusis)
/* print out one track as abc */
int trackno, barbeats, anacrusis;
{
  struct listx* i;
  int step, gap;
  int barnotes;
  int barcount;
  int breakcount;

  breakcount = 0;
  chordhead = NULL;
  chordtail = NULL;
  i = track[trackno].head;
  gap = 0;
  if (anacrusis > 0) {
    barnotes = anacrusis;
  } 
  else {
    barnotes = barbeats;
  };
  barcount = 0;
  while((i != NULL)||(gap != 0)) {
    if (gap == 0) {
      /* add notes to chord */
      addtochord(i->note);
      gap = i->note->xnum;
      i = i->next;
      advancechord(0); /* get rid of any zero length notes */
    } 
    else {
      step = findshortest(gap);
      if (step > barnotes) {
        step = barnotes;
      };
      if (step == 0) {
        fatal_error("Advancing by 0 in testtrack!");
      };
      advancechord(step);
      gap = gap - step;
      barnotes = barnotes - step;
      if (barnotes == 0) {
        if (chordhead != NULL) {
          breakcount = breakcount + 1;
        };
        barnotes = barbeats;
        barcount = barcount + 1;
        if (barcount>0  && barcount%4 ==0) {
        /* can't zero barcount because I use it for computing maxbarcount */
          freshline();
          barcount = 0;
        };
      };
    };
  };
  return(breakcount);
}

void printpitch(j)
/* convert numerical value to abc pitch */
struct anote* j;
{
  int p, po,i;

  p = j->pitch;
  if (p == -1) {
    fprintf(outhandle,"z");
  } 
  else {
    po = p % 12;
    if ((back[trans[p]] != p) || (key[po] == 1)) {
      fprintf(outhandle,"%c%c", symbol[po], atog[p]);
      for (i=p%12; i<256; i += 12) /* apply accidental to all octaves */
         back[trans[i]] = i;
    } 
    else {
      fprintf(outhandle,"%c", atog[p]);
    };
    while (p >= MIDDLE + 12) {
      fprintf(outhandle,"'");
      p = p - 12;
    };
    while (p < MIDDLE - 12) {
      fprintf(outhandle,",");
      p = p + 12;
    };
  };
}

static void reduce(a, b)
int *a, *b;
{
  int t, n, m;

    /* find HCF using Euclid's algorithm */
    if (*a > *b) {
        n = *a;
        m = *b;
      }
    else {
        n = *b;
        m = *a;
       };
while (m != 0) {
      t = n % m;
      n = m;
      m = t;
    };
*a = *a/n;
*b = *b/n;
}



void printfract(a, b)
/* print fraction */
/* used when printing abc */
int a, b;
{
  int c, d;

  c = a;
  d = b;
  reduce(&c,&d);
  /* print out length */
  if (c != 1) {
    fprintf(outhandle,"%d", c);
  };
  if (d != 1) {
    fprintf(outhandle,"/%d", d);
  };
}

void printchord(len)
/* Print out the current chord. Any notes that haven't            */
/* finished at the end of the chord are tied into the next chord. */
int len;
{
  struct dlistx* i;

  i = chordhead;
  if (i == NULL) {
    /* no notes in chord */
    fprintf(outhandle,"z");
    printfract(len, parts_per_unitlen);
    midline = 1;
  } 
  else {
    if (i->next == NULL) {
      /* only one note in chord */
      printpitch(i->note);
      printfract(len, parts_per_unitlen);
      midline = 1;
      if (len < i->note->playnum) {
        fprintf(outhandle,"-");
      };
    } 
    else {
      fprintf(outhandle,"[");
      while (i != NULL) {
        printpitch(i->note);
        printfract(len, parts_per_unitlen);
        if (len < i->note->playnum) {
          fprintf(outhandle,"-");
        };
        i = i->next;
      };
      fprintf(outhandle,"]");
      midline = 1;
    };
  };
}

char dospecial(i, barnotes, featurecount)
/* identify and print out triplets and broken rhythm */
struct listx* i;
int* barnotes;
int* featurecount;
{
  int v1, v2, v3, vt;
  int xa, xb;
  int pnum;
  long total, t1, t2, t3;

  
  if ((chordhead != NULL) || (i == NULL) || (i->next == NULL)
  /* || (asig%3 == 0) || (asig%2 != 0) 2004/may/09 SS*/) {
    return(' ');
  };
  t1 = i->note->dtnext;
  v1 = i->note->xnum;
  pnum = i->note->playnum;
  if ((v1 < pnum) || (v1 > 1 + pnum) || (pnum == 0)) {
    return(' ');
  };
  t2 = i->next->note->dtnext;
  v2 = i->next->note->xnum;
  pnum = i->next->note->playnum;
  if (/*(v2 < pnum) ||*/ (v2 > 1 + pnum) || (pnum == 0) || (v1+v2 > *barnotes)) {
    return(' ');
  };
  /* look for broken rhythm */
  total = t1 + t2;
  if (total == 0L) {
    /* shouldn't happen, but avoids possible divide by zero */
    return(' ');
  };
  if (((v1+v2)%2 == 0) && ((v1+v2)%3 != 0)) {
    vt = (v1+v2)/2;
      if (vt == validnote(vt)) {
      /* do not try to break a note which cannot be legally expressed */
      switch ((int) ((t1*6+(total/2))/total)) {
        case 2:
          *featurecount = 2;
          i->note->xnum  = vt;
          i->note->playnum = vt;
          i->next->note->xnum  = vt;
          i->next->note->playnum = vt;
          return('<');
          break;
        case 4:
          *featurecount = 2;
          i->note->xnum  = vt;
          i->note->playnum = vt;
          i->next->note->xnum  = vt;
          i->next->note->playnum = vt;
          return('>');
          break;
        default:
          break;
      };
    };
  };
  /* look for triplet */
  if (i->next->next != NULL) {
    t3 = i->next->next->note->dtnext;
    v3 = i->next->next->note->xnum;
    pnum = i->next->next->note->playnum;
    if ((v3 < pnum) || (v3 > 1 + pnum) || (pnum == 0) || 
        (v1+v2+v3 > *barnotes)) {
      return(' ');
    };
    if ((v1+v2+v3)%2 != 0) {
      return(' ');
    };
    vt = (v1+v2+v3)/2;
    if ((vt%2 == 1) && (vt > 1)) {
      /* don't want strange fractions in triplet */
      return(' ');
    };
    total = t1+t2+t3;
    xa = (int) ((t1*6+(total/2))/total); 
    xb = (int) (((t1+t2)*6+(total/2))/total);
    if ((xa == 2) && (xb == 4) && (vt%3 != 0) ) {
      *featurecount = 3;
      *barnotes = *barnotes + vt;
      i->note->xnum = vt;
      i->note->playnum = vt;
      i->next->note->xnum = vt;
      i->next->note->playnum = vt;
      i->next->next->note->xnum = vt;
      i->next->next->note->playnum = vt;
    };
  };
  return(' ');
}

int validnote(n)
int n;
/* work out a step which can be expressed as a musical time */
{
  int v;

  if (n <= 4) {
    v = n;
  } 
  else {
    v = 4;
    while (v*2 <= n) {
      v = v*2;
    };
    if (v + v/2 <= n) {
      v = v + v/2;
    };
  };
  return(v);
}

void handletext(t, textplace, trackno)
/* print out text occuring in the body of the track */
/* The text is printed out at the appropriate place within the track */
/* In addition the function handles key signature and time
/* signature changes that can occur in the middle of the tune. */
long t;
struct tlistx** textplace;
int trackno;
{
  char* str;
  char ch;
  int type,sf,mi,nn,denom,bb;

  while (((*textplace) != NULL) && ((*textplace)->when <= t)) {
    str = (*textplace)->text;
    ch = *str;
    type = (*textplace)->type;
    remove_carriage_returns(str);
    if (((int)ch == '\\') || ((int)ch == '/')) {
      inkaraoke = 1;
    };
    if ((inkaraoke == 1) && (karaoke == 1)) {
      switch(ch) {
        case ' ':
          fprintf(outhandle,"%s", str);
          midline = 1;
          break;
        case '\\':
          freshline();
          fprintf(outhandle,"w:%s", str + 1);
          midline = 1;
          break;
        case '/':
          freshline();
          fprintf(outhandle,"w:%s", str + 1);
          midline = 1;
          break;
        default :
          if (midline == 0) {
            fprintf(outhandle,"%%%s", str);
          } 
	  else {
            fprintf(outhandle,"-%s", str);
          };
          break;
      };
    } 
    else {
      freshline();
      ch=*(str+1);
      switch (type) {
      case 0:
      if (ch != '%') 
      fprintf(outhandle,"%%%s\n", str);
      else 
      fprintf(outhandle,"%s\n", str);
      break;
      case 1: /* key signature change */
      sscanf(str,"%d %d",&sf,&mi);
      if((trackno != 0 || trackcount==1) &&
	 (active_keysig != sf)) {
	      setupkey(sf);
	      active_keysig=sf;
              }
      break;
      case 2: /* time signature change */
      sscanf(str,"%d %d %d",&nn,&denom,&bb);
      if ((trackno != 0 || trackcount ==1) &&
	  (active_asig != nn || active_bsig != denom))
        {
  	setup_timesig(nn,denom,bb);
	fprintf(outhandle,"M: %d/%d\n",nn,denom);
        fprintf(outhandle,"L: 1/%d\n",unitlen);
	active_asig=nn;
	active_bsig=denom;
	}
      break;
     default:
      break;
      }
  }
  *textplace = (*textplace)->next;
 }
}

void printtrack(trackno, anacrusis)
/* print out one track as abc */
int trackno,  anacrusis;
{
  struct listx* i;
  struct tlistx* textplace;
  struct tlistx* textplace0; /* track 0 text storage */
  int step, gap;
  int barnotes;
  int barcount;
  int bars_on_line;
  long now;
  char broken;
  int featurecount;
  int last_barsize,barnotes_correction;

  midline = 0;
  featurecount = 0;
  inkaraoke = 0;
  now = 0L;
  broken = ' ';
  chordhead = NULL;
  chordtail = NULL;
  i = track[trackno].head;
  textplace = track[trackno].texthead;
  textplace0 = track[0].texthead;
  gap = track[trackno].startunits;
  if (anacrusis > 0) {
    barnotes = anacrusis;
    barcount = -1;
  } 
  else {
    barnotes = barsize;
    barcount = 0;
  };
  bars_on_line = 0;
  last_barsize = barsize;
  active_asig = header_asig;
  active_bsig = header_bsig;
  setup_timesig(header_asig,header_bsig,header_bb);
  active_keysig = header_keysig;
  handletext(now, &textplace, trackno);

  while((i != NULL)||(gap != 0)) {
    if (gap == 0) {
      /* do triplet here */
      if (featurecount == 0) {
        if (!no_triplets) {
          broken = dospecial(i, &barnotes, &featurecount);
        };
      };
      /* add notes to chord */
      addtochord(i->note);
      gap = i->note->xnum;
      now = i->note->time;
      i = i->next;
      advancechord(0); /* get rid of any zero length notes */
      if (trackcount > 1 && trackno !=0)
	      handletext(now, &textplace0, trackno);
      handletext(now, &textplace,trackno);
      barnotes_correction = barsize - last_barsize;
      barnotes += barnotes_correction;
      last_barsize = barsize;
    } 
    else {
      step = findshortest(gap);
      if (step > barnotes) {
        step = barnotes;
      };
      step = validnote(step);
      if (step == 0) {
        fatal_error("Advancing by 0 in printtrack!");
      };
      if (featurecount == 3)
        {
        fprintf(outhandle," (3");
        };
      printchord(step);
      if ( featurecount > 0) {
        featurecount = featurecount - 1;
      };
      if ((featurecount == 1) && (broken != ' ')) {
        fprintf(outhandle,"%c", broken);
      };
      advancechord(step);
      gap = gap - step;
      barnotes = barnotes - step;
      if (barnotes == 0) {
        fprintf(outhandle,"|");
        barnotes = barsize;
        barcount = barcount + 1;
	bars_on_line++;
        if (barcount >0 && barcount%bars_per_staff == 0)  {
		freshline();
		bars_on_line=0;
	}
     /* can't zero barcount because I use it for computing maxbarcount */
        else if(bars_on_line >= bars_per_line && i != NULL) {
		fprintf(outhandle," \\");
	       	freshline();
	        bars_on_line=0;}
      }
      else {
        if (featurecount == 0) {
          /* note grouping algorithm */
          if ((barsize/parts_per_unitlen) % 3 == 0) {
            if ( (barnotes/parts_per_unitlen) % 3 == 0
               &&(barnotes%parts_per_unitlen) == 0) {
              fprintf(outhandle," ");
            };
          } 
	  else {
            if (((barsize/parts_per_unitlen) % 2 == 0)
                && (barnotes % parts_per_unitlen) == 0
                && ((barnotes/parts_per_unitlen) % 2 == 0)) {
              fprintf(outhandle," ");
            };
          };
        };
      };
    };
  };
  /* print out all extra text */
  while (textplace != NULL) {
    handletext(textplace->when, &textplace, trackno);
  };
  freshline();
  if (barcount > maxbarcount) maxbarcount = barcount;
}




void remove_carriage_returns(char *str)
{
/* a carriage return might be embedded in a midi text meta-event.
   do not output this in the abc file or this would make a nonsyntactic
   abc file.
*/
char * loc;
while (loc  = (char *) strchr(str,'\r')) *loc = ' ';
while (loc  = (char *) strchr(str,'\n')) *loc = ' ';
}



void printQ()
/* print out tempo for abc */
{
  float Tnote, freq;
  Tnote = mf_ticks2sec((long)((xunit*unitlen)/4), division, tempo);
  freq = 60.0/Tnote;
  fprintf(outhandle,"Q:1/4=%d\n", (int) (freq+0.5));
  if (summary>0) printf("Tempo: %d quarter notes per minute\n",
    (int) (freq + 0.5));
}




void setupkey(sharps)
int sharps;
/* set up variables related to key signature */
{
  char sharp[13], flat[13], shsymbol[13], flsymbol[13];
  int j, t, issharp;
  int minkey;

  for (j=0; j<12; j++) 
    key[j] = 0;
  minkey = (sharps+12)%12;
  if (minkey%2 != 0) {
    minkey = (minkey+6)%12;
  };
  strcpy(sharp,    "ccddeffggaab");
  strcpy(shsymbol, "=^=^==^=^=^=");
  if (sharps == 6) {
    sharp[6] = 'e';
    shsymbol[6] = '^';
  };
  strcpy(flat, "cddeefggaabb");
  strcpy(flsymbol, "=_=_==_=_=_=");
  /* Print out key */

  if (sharps >= 0) {
    if (sharps == 6) {
      fprintf(outhandle,"K:F#");
    } 
    else {
      fprintf(outhandle,"K:%c", sharp[minkey] + 'A' - 'a');
    };
    issharp = 1;
  } 
  else {
    if (sharps == -1) {
      fprintf(outhandle,"K:%c", flat[minkey] + 'A' - 'a');
    } 
    else {
      fprintf(outhandle,"K:%cb", flat[minkey] + 'A' - 'a');
    };
    issharp = 0;
  };
  if (sharps >= 0) {
    fprintf(outhandle," %% %d sharps\n", sharps);
  }
  else {
    fprintf(outhandle," %% %d flats\n", -sharps);
  };
  key[(minkey+1)%12] = 1;
  key[(minkey+3)%12] = 1;
  key[(minkey+6)%12] = 1;
  key[(minkey+8)%12] = 1;
  key[(minkey+10)%12] = 1;
  for (j=0; j<256; j++) {
    t = j%12;
    if (issharp) {
      atog[j] = sharp[t];
      symbol[j] = shsymbol[t];
    } 
    else {
      atog[j] = flat[t];
      symbol[j] = flsymbol[t];
    };
    trans[j] = 7*(j/12)+((int) atog[j] - 'a');
    if (j < MIDDLE) {
      atog[j] = (char) (int) atog[j] + 'A' - 'a';
    };
    if (key[t] == 0) {
      back[trans[j]] = j;
    };
  };
}




/*  Functions for supporting the command line user interface to midi2abc.     */


int readnum(num) 
/* read a number from a string */
/* used for processing command line */
char *num;
{
  int t;
  char *p;
  int neg;
  
  t = 0;
  neg = 1;
  p = num;
  if (*p == '-') {
    p = p + 1;
    neg = -1;
  };
  while (((int)*p >= '0') && ((int)*p <= '9')) {
    t = t * 10 + (int) *p - '0';
    p = p + 1;
  };
  return neg*t;
}


int readnump(p) 
/* read a number from a string (subtly different) */
/* used for processing command line */
char **p;
{
  int t;
  
  t = 0;
  while (((int)**p >= '0') && ((int)**p <= '9')) {
    t = t * 10 + (int) **p - '0';
    *p = *p + 1;
  };
  return t;
}


void readsig(a, b, sig)
/* read time signature */
/* used for processing command line */
int *a, *b;
char *sig;
{
  char *p;
  int t;

  p = sig;
  if ((int)*p == 'C') {
    *a = 4;
    *b = 4;
    return;
  };
  *a = readnump(&p);
  if ((int)*p != '/') {
    char msg[80];

    sprintf(msg, "Expecting / in time signature found %c!", *p);
    fatal_error(msg);
  };
  p = p + 1;
  *b = readnump(&p);
  if ((*a == 0) || (*b == 0)) {
    char msg[80];

    sprintf(msg, "%d/%d is not a valid time signature!", *a, *b);
    fatal_error(msg);
  };
  t = *b;
  while (t > 1) {
    if (t%2 != 0) {
      fatal_error("Bad key signature, divisor must be a power of 2!");
    }
    else {
      t = t/2;
    };
  };
}

int is_power_of_two(int numb)
/* checks whether numb is a power of 2 less than 256 */
{
int i,k;
k = 1;
for (i= 0;i<8;i++) {
  if(numb == k) return(1);
  k *= 2;
  }
return(0);
}

int getarg(option, argc, argv)
/* extract arguments from command line */
char *option;
char *argv[];
int argc;
{
  int j, place;

  place = -1;
  for (j=0; j<argc; j++) {
    if (strcmp(option, argv[j]) == 0) {
      place = j + 1;
    };
  };
  return (place);
}

int huntfilename(argc, argv)
/* look for filename argument if -f option is missing */
/* assumes filename does not begin with '-'           */
char *argv[];
int argc;
{
  int j, place;

  place = -1;
  j = 1;
  while ((place == -1) && (j < argc)) {
    if (strncmp("-", argv[j], 1) != 0) {
      place = j;
    } 
    else {
     if (strchr("ambQkcou", *(argv[j]+1)) == NULL) {
       j = j + 1;
     }
     else {
       j = j + 2;
     };
    };
  };
  return(place);
}

int process_command_line_arguments(argc,argv)
char *argv[];
int argc;
{
  int val;
  int arg;
  arg = getarg("-ver",argc,argv);
  if (arg != -1) {printf("%s\n",VERSION); exit(0);}
  midiprint = 0;
  arg = getarg("-Midigram",argc,argv);
  if (arg != -1) 
   {
   midiprint=1;
/*   return arg; */
   }

  arg = getarg("-a", argc, argv);
  if ((arg != -1) && (arg < argc)) {
    anacrusis = readnum(argv[arg]);
  } 
  else {
    anacrusis = 0;
  };
  arg = getarg("-m", argc, argv);
  if ((arg != -1) && (arg < argc)) {
    readsig(&asig, &bsig, argv[arg]);
    tsig_set = 1;
  }
  else {
    asig = 4;
    bsig = 4;
    tsig_set = 0;
  };
  arg = getarg("-Q", argc, argv);
  if (arg != -1) {
    Qval = readnum(argv[arg]);
  }
  else {
    Qval = 0;
  };
  arg = getarg("-u", argc,argv);
  if (arg != -1) {
    xunit = readnum(argv[arg]);
    xunit_set = 1;
  }
  else {
	xunit = 0;
	xunit_set = 0;
       };
  arg = getarg("-ppu",argc,argv);
  if (arg != -1) {
     val = readnum(argv[arg]);
     if (is_power_of_two(val)) parts_per_unitlen = val;
     else {
	   printf("*error* -ppu parameter must be a power of 2\n");
           parts_per_unitlen = 2;
          }
  }
  else
     parts_per_unitlen = 2;
  arg = getarg("-aul",argc,argv);
  if (arg != -1) {
     val = readnum(argv[arg]);
     if (is_power_of_two(val)) {
	    unitlen = val;
	    unitlen_set = 1;}
        else 
           printf("*error* -aul parameter must be a power of 2\n");
      }
  arg = getarg("-bps",argc,argv);
  if (arg != -1)
   bars_per_staff = readnum(argv[arg]);
  else {
       bars_per_staff=4;
   };
  arg = getarg("-bpl",argc,argv);
  if (arg != -1)
   bars_per_line = readnum(argv[arg]);
  else {
       bars_per_line=1;
   };


  extracta = (getarg("-xa", argc, argv) != -1);
  guessa = (getarg("-ga", argc, argv) != -1);
  guessu = (getarg("-gu", argc, argv) != -1);
  guessk = (getarg("-gk", argc, argv) != -1);
  keep_short = (getarg("-s", argc, argv) != -1);
  summary = getarg("-sum",argc,argv); 
  swallow_rests = getarg("-sr",argc,argv);
  if (swallow_rests != -1) {
	 restsize = readnum(argv[swallow_rests]);
         if(restsize <1) restsize=1;
        }
  obpl = getarg("-obpl",argc,argv);
  if (obpl>= 0) bars_per_line=1;
  if (!unitlen_set) {
    if ((asig*4)/bsig >= 3) {
      unitlen =8;
     }
    else {
      unitlen = 16;
     };
   }
  arg = getarg("-b", argc, argv);
  if ((arg != -1) && (arg < argc)) {
    bars = readnum(argv[arg]);
  }
  else {
    bars = 0;
  };
  arg = getarg("-c", argc, argv);
  if ((arg != -1) && (arg < argc)) {
    xchannel = readnum(argv[arg]) - 1;
  }
  else {
    xchannel = -1;
  };
  arg = getarg("-k", argc, argv);
  if ((arg != -1) && (arg < argc)) {
    keysig = readnum(argv[arg]);
    if (keysig<-6) keysig = 12 - ((-keysig)%12);
    if (keysig>6)  keysig = keysig%12;
    if (keysig>6)  keysig = keysig - 12;
    ksig_set = 1;
  } 
  else {
    keysig = -50;
    ksig_set = 0;
  };

  if(guessk) ksig_set=1;

  arg = getarg("-o",argc,argv);
  if ((arg != -1) && (arg < argc))  {
    outhandle = efopen(argv[arg],"w");  /* open output abc file */
  } 
  else {
    outhandle = stdout;
  };
  arg = getarg("-nt", argc, argv);
  if (arg == -1) {
    no_triplets = 0;
  } 
  else {
    no_triplets = 1;
  };
  arg = getarg("-f", argc, argv);
  if (arg == -1) {
    arg = huntfilename(argc, argv);
  };
  if ((arg != -1) && (arg < argc)) {
    F = efopen(argv[arg],"rb");
/*    fprintf(outhandle,"%% input file %s\n", argv[arg]); */
  }
  else {
    printf("midi2abc version %s\n  usage :\n",VERSION);
    printf("midi2abc filename <options>\n");
    printf("         -a <beats in anacrusis>\n");
    printf("         -xa  extract anacrusis from file ");
    printf("(find first strong note)\n");
    printf("         -ga  guess anacrusis (minimize ties across bars)\n");
    printf("         -gk  guess key signature \n");
    printf("         -gu  guess xunit from note duration statistics\n");
    printf("         -m <time signature>\n");
    printf("         -b <bars wanted in output>\n");
    printf("         -Q <tempo in quarter-notes per minute>\n");
    printf("         -k <key signature> -6 to 6 sharps\n");
    printf("         -c <channel>\n");
    printf("         -u <number of midi pulses in abc time unit>\n");
    printf("         -ppu <number of parts in abc time unit>\n");
    printf("         -aul <denominator of L: unit length>\n");
    printf("         [-f] <input file>\n");
    printf("         -o <output file>\n");
    printf("         -s do not discard very short notes\n");
    printf("         -sr <number> absorb short rests following note\n");
    printf("           where <number> specifies its size\n");
    printf("         -sum summary\n");
    printf("         -nt do not look for triplets or broken rhythm\n");
    printf("         -bpl <number> of bars printed on one line\n");
    printf("         -bps <number> of bars to be printed on staff\n");
    printf("         -obpl one bar per line\n");
    printf("         -Midigram   Prints midigram instead of abc file\n");
    printf("         -ver version number\n");
    printf(" None or only one of the options -gu, -b, -Q -u should\n");
    printf(" be specified. If none are present, midi2abc will uses the\n");
    printf(" the PPQN information in the MIDI file header to determine\n");
    printf(" the suitable note length. This is the recommended method.\n");
    printf(" The input filename is assumed to be any string not\n");
    printf(" beginning with a - (hyphen). It may be placed anywhere.\n");

    exit(0);
  };
  return arg;
}



void midi2abc (arg, argv)
char *argv[];
int arg;
{
int voiceno;
int accidentals; /* used for printing summary */
int j;
int argc;

/* initialization */
  trackno = 0;
  track[trackno].texthead = NULL;
  track[trackno].texttail = NULL;
  initfuncs();
  playinghead = NULL;
  playingtail = NULL;
  karaoke = 0;
  Mf_getc = filegetc;

/* parse MIDI file */
  mfread();

  fclose(F);

/* count MIDI tracks with notes */
  maintrack = 0;
  while ((track[maintrack].notes == 0) && (maintrack < trackcount)) {
    maintrack = maintrack + 1;
  };
  if (track[maintrack].notes == 0) {
    fatal_error("MIDI file has no notes!");
  };

/* compute dtnext for each note */
  for (j=0; j<trackcount; j++) {
    postprocess(j);
  };

  if (tsig_set == 1){  /* for -m parameter set up time signature*/
       header_asig = asig; 
       header_unitlen = unitlen;
       header_bsig = bsig;
  };

/* print abc header block */
  argc = huntfilename(arg, argv);
  fprintf(outhandle,"X: 1\n"); 
  fprintf(outhandle,"T: from %s\n",argv[argc]); 
  fprintf(outhandle,"M: %d/%d\n", header_asig, header_bsig);
  fprintf(outhandle,"L: 1/%d\n", header_unitlen); 

  barsize = parts_per_unitlen*header_asig*header_unitlen/header_bsig;

/* compute xunit size for -Q -b options */
  if (Qval != 0) {
    xunit = mf_sec2ticks((60.0*4.0)/((float)(Qval*unitlen)), division, tempo);
  };
  if (bars > 0) {
    xunit = (int) ((float) track[maintrack].tracklen*2/(float) (bars*barsize));
    /* we multiply by 2 because barsize is in half unitlen. */
  };

/* estimate xunit if not set or if -gu runtime option */
  if (xunit == 0 || guessu) {
    guesslengths(maintrack);
  };

/* output Q: (tempo) to abc file */
  printQ();

/* Quantize note lengths of all tracks */
  for (j=0; j<trackcount; j++) {
    quantize(j, xunit);
  };

/* Estimate anacrusis if requested. Otherwise it is set by       */
/* user or set to 0.                                             */
  if (extracta) {
    anacrusis = findana(maintrack, barsize);
    fprintf(outhandle,"%%beats in anacrusis = %d\n", anacrusis);
  };
  if (guessa) {
    anacrusis = guessana(barsize);
    fprintf(outhandle,"%%beats in anacrusis = %d\n", anacrusis);
  };

/* If key signature is not known find the best one.              */
  if (keysig == -50 && gotkeysig ==0 || guessk) {
       keysig = findkey(maintrack);
       if (summary>0) printf("Best key signature = %d flats/sharps\n",keysig);
       }
       header_keysig = keysig;
       setupkey(header_keysig);

/* scannotes(maintrack); For debugging */

/* Convert each track to abc format and print */
  if (trackcount > 1) {
    voiceno = 1;
    for (j=0; j<trackcount; j++) {
      freshline();
      if (track[j].notes > 0) {
        fprintf(outhandle,"V:%d\n", voiceno);
	if (track[j].drumtrack) fprintf(outhandle,"%%%%MIDI channel 10\n");
        voiceno = voiceno + 1;
      };
      printtrack(j,anacrusis);
    };
  }
  else {
    printtrack(maintrack, anacrusis);
  };

  /* scannotes(maintrack); for debugging */

/* output report if requested */
  if(summary>0) {
   accidentals = keysig;
   if (accidentals <0 )
     {
     accidentals = -accidentals;
     printf("Using key signature: %d flats\n", accidentals);
     }
   else
     printf("Using key signature : %d sharps\n", accidentals);
   printf("Using an anacrusis of %d beats\n",anacrusis);
   printf("Using unit length : 1/%d\n",unitlen);
   printf("Using %d pulses per unit length (xunit).\n",xunit);
   printf("Producing %d bars of output.\n",maxbarcount);
   }


/* free up data structures */
  for (j=0; j< trackcount; j++) {
    struct listx* this;
    struct listx* x;
    struct tlistx* tthis;
    struct tlistx* tx;

    this = track[j].head;
    while (this != NULL) {
      free(this->note);
      x = this->next ;
      free(this);
      this = x;
    };
    tthis = track[j].texthead;
    while (tthis != NULL) {
      free(tthis->text);
      tx = tthis->next;
      free(tthis);
      tthis = tx;
    };
  };
  fclose(outhandle);
}

void midigram(argc,argv)
char *argv[];
int argc;
{
initfunc_for_midinotes();
init_notechan();
last_tick=0;
/*F = efopen(argv[argc -1],"rb");*/
Mf_getc = filegetc;
mfread();
printf("%d\n",last_tick);
}


int main(argc,argv)
char *argv[];
int argc;
{
  FILE *efopen();
  int arg;

  arg = process_command_line_arguments(argc,argv);
  if(midiprint) midigram(argc,argv);
  else midi2abc(argc,argv); 
  return 0;
}
