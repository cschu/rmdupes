#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUGMODE 0
#define MAXREADLEN 128
#define MAXSEQLEN 99

int ALLOW_MODIFY = 1;

typedef struct NANodeLabel {
  int nseqs;
  int seqid;
  char * qualstr;  
} NANodeLabel;

typedef struct NATrie {
  struct NANodeLabel *label;
  struct NATrie *A, *C, *G, *T, *sep;
} NATrie;

int seq_contains_N(char * seq) {
  char * lp = seq;
  while(*lp) {
    if (*(lp++) == 'N') {
      return 1;
    }
  }
  return 0;
}

void cut_string_after_first_space(char *seq) {
  char * lp = seq;
  while(*lp && !isspace(*(lp++))) ;
  //was: *(--lp) = 0;    // why --?
  *lp = 0; 
  return ;
}

int init(struct NATrie * node) {
  node = malloc(sizeof(NATrie));
  node->A = NULL;
  node->C = NULL;
  node->G = NULL;
  node->T = NULL;
  node->sep = NULL;
  node->label = NULL;
  return 0;
}

struct NATrie * findChild(struct NATrie * node, char c) {
  switch(c) {
  case 'A':
    return node->A;
    break;
  case 'C':
    return node->C;
    break;
  case 'G':
    return node->G;
    break;
  case 'T':
    return node->T;
    break;
  case ':':
    return node->sep;
    break;    
  default:
    return NULL;
  } 
}

struct NATrie * attachChild(struct NATrie * node, char c) {
  NATrie * child = (NATrie*) malloc(sizeof(NATrie));
  switch(c) {
  case 'A':
    node->A = child;
    break;
  case 'C':
    node->C = child;
    break;
  case 'G':
    node->G = child;
    break;
  case 'T':
    node->T = child;
    break;
  case ':':
    node->sep = child;
    break;
  default:
    free(child);
    child = NULL;
  }
  return child;
}
 
struct NATrie * lookup(struct NATrie * node, char * string) {
  int len = (int)strlen(string);
#if DEBUGMODE
  printf("LOOKUP: %s$ (%i)\n", string, len);
#endif
  // find edge corresponding to current base
  struct NATrie * next = findChild(node, *string);  
  if (NULL == next) {
#if DEBUGMODE
    printf("nirvana.\n");
#endif
    // edge ends in nirvana
    if (len > 0) {
      // string has not been found in tree
      return NULL;
    } else {
#if DEBUGMODE
      if (NULL == node) printf("NODENULL\n");
#endif
      // string has been found
      return node;
    }
  } else {
    // continue traversal
    return lookup(next, ++string);
  }
}

struct NANodeLabel * insert(struct NATrie * node, char * string, int seqid) {
  //printf("INSERTx:%s$,%i\n", string, seqid);
  if ((int)strlen(string) == 0) {
    // Add label to current node
    node->label = (NANodeLabel*) malloc(sizeof(NANodeLabel*));
    node->label->nseqs = 1;
    node->label->seqid = seqid;
    node->label->qualstr = "";
    return node->label;
  }  
  
  // find edge corresponding to current base
  struct NATrie * next = findChild(node, *string);
  if (NULL == next) {
    // edge ends in nirvana: add downstream node
    next = attachChild(node, *string);
    init(next);
  }
  // continue traversal
  return insert(next, ++string, seqid);
}

void getSequences(struct NATrie * node, char * seq, char * mainseq,
		  FILE * fpo1, FILE * fpo2) {
  char buffer[2*MAXREADLEN];
  char *pbuf = buffer, *sbuf = mainseq;
  if (NULL != node->label) {
    // node has a label, i.e. a sequence ends here
    *seq = 0; // set current (last) position to 0
    // printf(">UNIQUE_SEQUENCE_%010i_NSEQS=%i_LEN=%i\n%s\n", node->label->seqid, node->label->nseqs, mainseq);
    while(*sbuf != ':') {
      *(pbuf++) = *(sbuf++);
    }
    *pbuf = 0;
    fprintf(fpo1, ">UNIQSEQ_%010i_LEN=%i_NSEQS=%i\n%s\n", node->label->seqid, strlen(buffer), node->label->nseqs, buffer);
    pbuf = buffer;
    ++sbuf;
    while(*sbuf != 0) {
      *(pbuf++) = *(sbuf++);
    }
    *pbuf = 0;
    fprintf(fpo2, ">UNIQSEQ_%010i_LEN=%i_NSEQS=%i\n%s\n", node->label->seqid, strlen(buffer), node->label->nseqs, buffer);
  }
  if (NULL != node->A) {
    *(seq++) = 'A'; // set current position to 'A', then increment
    getSequences(node->A, seq, mainseq, fpo1, fpo2); // traverse ->A-subtree
    --seq; // decrement for next child    
  }
  if (NULL != node->C) {
    *(seq++) = 'C'; // set current position to 'C', then increment
    getSequences(node->C, seq, mainseq, fpo1, fpo2); // traverse ->C-subtree
    --seq; // decrement for next child    
  }
  if (NULL != node->G) {
    *(seq++) = 'G'; // set current position to 'G', then increment
    getSequences(node->G, seq, mainseq, fpo1, fpo2); // traverse ->G-subtree
    --seq; // decrement for next child    
  }
  if (NULL != node->T) {
    *(seq++) = 'T'; // set current position to 'T', then increment
    getSequences(node->T, seq, mainseq, fpo1, fpo2); // traverse ->T-subtree
    --seq; // decrement for next child    
  }
  if (NULL != node->sep) {
    *(seq++) = ':'; // set current position to separator, then increment
    getSequences(node->sep, seq, mainseq, fpo1, fpo2); // traverse ->separator-subtree
    --seq; // decrement for next child    
  }
  return ;
}

void remove_newline(char * line) {
  int len = (int)strlen(line) - 1;
  if(line[len] == '\n') { line[len] = 0; }
  return ;
}


void processReads(struct NATrie * trie, FILE * fp1, FILE * fp2,
		  FILE * fpo1, FILE * fpo2) {
  char line1[MAXREADLEN], line2[MAXREADLEN];
  int lct = -1;
  char fqid[MAXREADLEN];
  struct NATrie * lookup_value;
  char seq1[2*MAXREADLEN], seq2[2*MAXREADLEN];
  int allow_printing = 0;
  int readcount = 0;
  int uniqcount = 0;
  int n_in_seq = 0;
  int seqs_with_n = 0;
  
  while (1) {
    if (NULL == fgets(line1, sizeof(line1), fp1)) { break; }
    fgets(line2, sizeof(line2), fp2);
    remove_newline(line1);
    remove_newline(line2);
    lct += 1;

#if DEBUGMODE
    printf("L1:%s$\nL2:%s$\n", line1, line2);
    printf("LCT:%i (%i)\n", lct, lct % 4);
#endif

    if (lct % 4 == 0) {
      // FASTQID
      n_in_seq = 0;
      if (allow_printing) {
	allow_printing = 0;
      }

      cut_string_after_first_space(line1);
      cut_string_after_first_space(line2);
      
      if (strncmp(line1, line2, strlen(line1))) {
	printf("ERROR: FastQ identifiers do not match: $%s$ $%s$ %i %i\n", 
	       line1, line2, (int)strlen(line1), (int)strlen(line2));
	exit(EXIT_FAILURE);
      }
      sprintf(fqid, line1);
      
    } else if (lct % 4 == 1) {
      // SEQUENCE
      n_in_seq = seq_contains_N(line1) || seq_contains_N(line2);
      if (n_in_seq) { 
	++seqs_with_n;
	continue; 
      }
      ++readcount;
      
      if (MAXSEQLEN < strlen(line1)) { line1[MAXSEQLEN] = 0; }
      if (MAXSEQLEN < strlen(line2)) { line2[MAXSEQLEN] = 0; }


#if DEBUGMODE
      printf("Performing FWD-lookup\n");
#endif
      sprintf(seq1, "%s:%s", line1, line2);
      lookup_value = lookup(trie, seq1);
      if (NULL == lookup_value) {
	// seq1:seq2 is unknown => check seq2:seq1
#if DEBUGMODE
	printf("Performing REV-lookup\n");
#endif	
	sprintf(seq2, "%s:%s", line2, line1);
	lookup_value = lookup(trie, seq2);
	if (NULL == lookup_value) {
	  //insert into trie (label: id from readcount)
#if DEBUGMODE
	  printf(">>>INSERTING %s with id=%i\n", seq1, readcount - 1);
#endif
	  insert(trie, seq1, readcount - 1); 
#if DEBUGMODE
	  printf("BONE\n");
#endif
	  allow_printing = 1;
	  ++uniqcount;
#if 1
	  printf("\r#Unique/Total sequences: %i/%i (%.5f, d=%i)",
		 uniqcount, readcount, (double)uniqcount/readcount, readcount - uniqcount);
	  fflush(stdout);
#endif
	  fprintf(fpo1, "%s\n%s\n", fqid, line1);
	  fprintf(fpo2, "%s\n%s\n", fqid, line2);
	  fflush(fpo1);
	  fflush(fpo2);
	  
	} 
      }
      
      if (NULL != lookup_value) {
	// string=seq1:seq2 or string=seq2:seq1 were found in the trie
	if (NULL == lookup_value->label) {
	  // string ended at internal node, i.e. has no label yet
	  lookup_value->label = (NANodeLabel*) malloc(sizeof(NANodeLabel*));
	  lookup_value->label->nseqs = 0;
	  lookup_value->label->seqid = readcount - 1;
	}
	++(lookup_value->label->nseqs);

      }

    } else if (!n_in_seq && allow_printing) {
      if (MAXSEQLEN < strlen(line1)) { line1[MAXSEQLEN] = 0; }
      if (MAXSEQLEN < strlen(line2)) { line2[MAXSEQLEN] = 0; }

      fprintf(fpo1, "%s\n", line1);
      fprintf(fpo2, "%s\n", line2);
      fflush(fpo1);
      fflush(fpo2);
    }      
  } // end while
  printf("\nDropped %i sequence(s) containing N.\n", seqs_with_n);

  return ;
}




int main(int argc, char * argv[]) {
  
  char *fn1 = argv[1];
  char *fn2 = argv[2];
  char *fo1 = argv[3];
  char *fo2 = argv[4];
  char *fowc1 = argv[5];
  char *fowc2 = argv[6];

  FILE *fp1 = fopen(fn1, "r");
  FILE *fp2 = fopen(fn2, "r");
  FILE *fpo1 = fopen(fo1, "w");
  FILE *fpo2 = fopen(fo2, "w");

  if (NULL == fp1) {
    printf("File failed to open: %s\n", fn1);
    exit(EXIT_FAILURE);
  }
  if (NULL == fp2) {
    printf("File failed to open: %s\n", fn2);
    exit(EXIT_FAILURE);  
  }
  if (NULL == fpo1) {
    printf("File failed to open: %s\n", fo1);
    exit(EXIT_FAILURE);
  }
  if (NULL == fpo2) {
    printf("File failed to open: %s\n", fo2);
    exit(EXIT_FAILURE);  
  }
  
  struct NATrie * root = (NATrie*) malloc(sizeof(NATrie));
  init(root);
  processReads(root, fp1, fp2, fpo1, fpo2);

  fclose(fpo1);
  fclose(fpo2);  
  fclose(fp1);
  fclose(fp2);

  fpo1 = fopen(fowc1, "w");
  fpo2 = fopen(fowc2, "w");

  if (NULL == fpo1) {
    printf("File failed to open: %s\n", fo1);
    exit(EXIT_FAILURE);
  }
  if (NULL == fpo2) {
    printf("File failed to open: %s\n", fo2);
    exit(EXIT_FAILURE);  
  }  
  
  char * mainseq = (char*) malloc(sizeof(char) * MAXREADLEN * 2);
  char * outseq = mainseq;
  getSequences(root, outseq, mainseq, fpo1, fpo2);
  fclose(fpo1);
  fclose(fpo2);  


#if 0
  char * s = "CA";
  insert(root, s, 3);
  printf("Size:%i, Total:%i, root:%i\n", sizeof(NATrie), sizeof(*root->C), sizeof(*root));

  s = "ACGT";
  insert(root, s, 1);
  s = "ACGA";
  insert(root, s, 2);

  printf("Lookup ACGT: %i\n",  lookup(root, "ACGT"));
  printf("Lookup ACGA: %i\n",  lookup(root, "ACGA"));
  printf("Lookup ACG: %i\n",  lookup(root, "ACG"));
  printf("Lookup G: %i\n",  lookup(root, "G"));
  printf("Lookup A: %i\n",  lookup(root, "A"));
#endif


#if 0
  root->A = (NATrie*) malloc(sizeof(NATrie));
  root->A->label = 16;
  
  char * r = "A";
  printf("Label found: %i\n", lookup(root, r));


  char * s = "ACGT";
  printf("test.\n");  
  int lbl = insert(root, s, 14);
  printf("Label: %i\n", lbl);

  int lookup_lbl = lookup(root, s);//, strlen(s));
  printf("Label found: %i\n", lookup_lbl);

  s = "C";
  printf("Test:%i\n", lookup(root, s));//, 1));
 #endif
  
#if 0 
  struct NATrie * child = (NATrie*) malloc(sizeof(NATrie));
  root->A = child;
  //root->A = malloc(sizeof(NATrie));

  init(child);
  child = (NATrie*) malloc(sizeof(NATrie));
  init(child);
  root->A->C = child;
  root->C = (NATrie*) malloc(sizeof(NATrie));
  init(root->C);


  char * s = "C";
  printf("Lookup:%i\n", lookup(root, s, strlen(s))); 
  printf("Size:%i, Total:%i\n", sizeof(NATrie), sizeof(root->C));

  char * r = "XYZ";
  printf("strlen(%s) = %i\n", r, strlen(r));
  char * rr = ++r;
  printf("strlen(%s) = %i\n", rr, strlen(rr));
#endif  
  return 0;
}



#if 0
char * trim(char * string) {
  //from http://stackoverflow.com/questions/122616/how-do-i-trim-leading-trailing-whitespace-in-a-standard-way
  char *end;
  while (isspace(*string)) ++string;
  if (*string == 0) {
    return string;
  }
  end = string + strlen(string) - 1;
  while (end > string && isspace(*end)) --end;

  *(end + 1) = 0;
  return string;
}
#endif

