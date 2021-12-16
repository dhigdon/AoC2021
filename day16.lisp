;;; Day 15 - Packet Decoder

;; -----------------------------------------------------------
;; Read the data into a bitvector for the decoder

(defun fill-char (c vec)
  "Puts the bits represented by the hex digit c into vec starting with the MSB"
  (let ((v (digit-char-p c 16)))
    ; unrolled because ldb compiles very efficiently this way
    (vector-push (ldb (byte 1 3) v) vec)
    (vector-push (ldb (byte 1 2) v) vec)
    (vector-push (ldb (byte 1 1) v) vec)
    (vector-push (ldb (byte 1 0) v) vec)))

(defun to-bits (packet)
  "Return a bit vector of the bits encoded in the packet string"
  (loop
    with result = (make-array (* 4 (length packet))
                              :fill-pointer 0
                              :element-type '(integer 0 1)) 
    for c across packet
    do (fill-char c result)
    finally (return result)))

;; -----------------------------------------------------------
;; To decode the packet, we have this thing

(defstruct decode
  packet
  (pos 0 :type fixnum))

(defun decode-eof-p (decoder)
  "Have we reached the end of the packet?"
  (= (decode-pos decoder)
     (length (decode-packet decoder))))

(defun decode-bit (decoder)
  "Return the next bit, advancing the read position"
  (unless (decode-eof-p decoder)
    (prog1
      (aref (decode-packet decoder) (decode-pos decoder))
      (incf (decode-pos decoder)))))

(defun decode-int (decoder bits)
  "Decode 'bits' bits into an integer"
  (let ((result 0))
    (dotimes (n bits result)
      (setf result (+ (decode-bit decoder) result result)))))

;; -----------------------------------------------------------
;; Decoding the packet data itself

(defstruct packet kind version contents)

(defun decode-literal-int (decoder)
  "Read an encoded literal, which consists
  of a sequence of 5-bit values.
  [MARK:1][VAL:4].
  When MARK is 1, there are more digits to follow.
  The digits are big-endian, and combine to form
  an integer value."
  (let ((result 0))
    (loop for mark = (decode-int decoder 1)
          and val  = (decode-int decoder 4)
          do (setf result (+ (* 16 result) val))
          until (zerop mark))
    result))

;; Forward declaration
(declaim (ftype (function (*) *) read-packet))

(defun read-subpacket-count (d n)
  "Read a number of packets"
  (loop for i below n
        collecting (read-packet d)))

(defun read-subpacket-length (d n)
  (loop with end = (+ n (decode-pos d))
        until (= (decode-pos d) end)
        collecting (read-packet d)))

(defun kind-op (kind)
  "Return a function to evaluate a given kind of packet"
  (case kind
    (0 #'+)
    (1 #'*)
    (2 #'min)
    (3 #'max)
    (4 'literal) ; NOTE: not a function
    (5 (lambda (a b) (if (> a b) 1 0)))
    (6 (lambda (a b) (if (< a b) 1 0)))
    (7 (lambda (a b) (if (= a b) 1 0)))
    (otherwise 'error)))

(defun read-packet (d)
  (let ((version (decode-int d 3))
        (kind (kind-op (decode-int d 3))))
    (make-packet
      :version version
      :kind kind 
      :contents (cond ((eq kind 'literal)
                       (decode-literal-int d))

                      ;; The rest are operators, but their parameters
                      ;; come in two flavors, chosen by the next bit.
                      ((= 1 (decode-int d 1))
                       (read-subpacket-count d (decode-int d 11)))

                      (t (read-subpacket-length d (decode-int d 15)))))))

(defun to-packet (str)
  (read-packet (make-decode :packet (to-bits str))))

;; -----------------------------------------------------------

;; NOTE: the following two functions could be more efficient and
;; not use an intermediate list between reduce and mapcar.

(defun eval-packet (pkt)
  "Execute the encoded expression"
  (let ((k (packet-kind pkt)))
    (if (eq 'literal k)
      (packet-contents pkt)
      (reduce k (mapcar #'eval-packet (packet-contents pkt))))))

(defun count-versions (pkt)
  "Sum the version fields of all packets and sub-packets"
  (let ((k (packet-kind pkt)))
    (if (eq 'literal k)
      (packet-version pkt)
      (+ (packet-version pkt)
         (reduce #'+ (mapcar #'count-versions
                             (packet-contents pkt)))))))

;; -----------------------------------------------------------

(defvar data "A20D6CE8F00033925A95338B6549C0149E3398DE75817200992531E25F005A18C8C8C0001849FDD43629C293004B001059363936796973BF3699CFF4C6C0068C9D72A1231C339802519F001029C2B9C29700B2573962930298B6B524893ABCCEC2BCD681CC010D005E104EFC7246F5EE7328C22C8400424C2538039239F720E3339940263A98029600A80021B1FE34C69100760B41C86D290A8E180256009C9639896A66533E459148200D5AC0149D4E9AACEF0F66B42696194031F000BCE7002D80A8D60277DC00B20227C807E8001CE0C00A7002DC00F300208044E000E69C00B000974C00C1003DC0089B90C1006F5E009CFC87E7E43F3FBADE77BE14C8032C9350D005662754F9BDFA32D881004B12B1964D7000B689B03254564414C016B004A6D3A6BD0DC61E2C95C6E798EA8A4600B5006EC0008542D8690B80010D89F1461B4F535296B6B305A7A4264029580021D1122146900043A0EC7884200085C598CF064C0129CFD8868024592FEE9D7692FEE9D735009E6BBECE0826842730CD250EEA49AA00C4F4B9C9D36D925195A52C4C362EB8043359AE221733DB4B14D9DCE6636ECE48132E040182D802F30AF22F131087EDD9A20804D27BEFF3FD16C8F53A5B599F4866A78D7898C0139418D00424EBB459915200C0BC01098B527C99F4EB54CF0450014A95863BDD3508038600F44C8B90A0801098F91463D1803D07634433200AB68015299EBF4CF5F27F05C600DCEBCCE3A48BC1008B1801AA0803F0CA1AC6200043A2C4558A710E364CC2D14920041E7C9A7040402E987492DE5327CF66A6A93F8CFB4BE60096006E20008543A8330780010E8931C20DCF4BFF13000A424711C4FB32999EE33351500A66E8492F185AB32091F1841C91BE2FDC53C4E80120C8C67EA7734D2448891804B2819245334372CBB0F080480E00D4C0010E82F102360803B1FA2146D963C300BA696A694A501E589A6C80")

(defun part1 (d)
  (count-versions (to-packet d)))

(defun part2 (d)
  (eval-packet (to-packet d)))
