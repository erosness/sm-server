;;; ==================== FNV hash ====================
;;; Fowler-Noll-Vo hash algorithm. 32-bit. Simple and nice
;;; distribution. Reference C implementation here:
;;; http://www.isthe.com/chongo/tech/comp/fnv/index.html#FNV-reference-source,
;;; where you can: $ printf abba | ./fnv132 => 0xb8de711f
;;; (number->string (fnv1-32 "abba") 16)    =>  "b8de711f"
;;;
;;; From https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function:
;;;
;;; hash = FNV_offset_basis
;;;   for each byte_of_data to be hashed
;;;        hash = hash × FNV_prime
;;;        hash = hash XOR byte_of_data
;;;   return hash
;;;

(define (fnv1-32 str)
  ((foreign-lambda* int ((c-string str))
                    "
  int hval = 0x811c9dc5; // <- hash init FNV1_32_INIT
  unsigned char *s = (unsigned char *)str;
  while (*s) {
    hval *= 0x01000193; // FNV_32_PRIME
    hval ^= (int)*s++;
  }
  return(hval);") str))
