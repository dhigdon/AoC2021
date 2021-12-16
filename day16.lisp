;;; Day 15 - Packet Decoder

(defun digit-bits (c)
  "Return a bitvector of the bits that make up hex digit c"
  (when (digit-char-p c 16)
    (let ((bits (make-array 4 :element-type '(integer 0 1)))
          (rec (format nil "~4B" (digit-char-p c 16))))
      (dotimes (i 4 bits)
        (setf (aref bits i)
              (if (char= #\1 (char rec i)) 1 0))))))

(defun fill-char (c vec)
  (loop for d across (digit-bits c) do (vector-push d vec)))

(defun to-bits (packet)
  (let ((result (make-array (* 4 (length packet))
                            :fill-pointer 0
                            :element-type '(integer 0 1))))
    (loop for c across packet do (fill-char c result))
    result))

; To decode the packet, we have this thing

(defstruct decode
  packet
  (pos 0 :type fixnum))

(defun decode-eof (decode)
  (= (decode-pos decode)
     (length (decode-packet decode))))

(defun read-bit (decoder)
  (unless (decode-eof decoder)
    (prog1
      (aref (decode-packet decoder) (decode-pos decoder))
      (incf (decode-pos decoder)))))

(defun read-bits (decoder bits)
  (unless (decode-eof decoder)
    (let ((result (make-array bits :element-type '(integer 0 1))))
      (dotimes (b bits result)
        (setf (aref result b) (read-bit decoder))))))

(defun read-tail (decoder)
  "Reads up to the next 4-bit alignment"
  (let ((remains (nth-value 1 (ceiling (decode-pos decoder) 4))))
    (read-bits decoder (- remains))))

(defun to-int (bits)
  (declare (bit-vector bits))
  (let ((result 0))
    (loop for b across bits
          do (setf result (+ b result result)))
    result))

(defun read-int (decoder bits)
  (to-int (read-bits decoder bits)))

;; Decoding the packet data itself

(defstruct packet
  kind
  version
  contents)

(defun read-literals (d)
  (if (= 1 (read-int d 1))
    (cons (read-int d 4) (read-literals d))
    (cons (read-int d 4) nil)))

(defun read-literal-int (decoder)
  (let ((result 0))
    (dolist (d (read-literals decoder))
      (setf result (+ (* 16 result) d)))
    result))

(declaim (ftype (function (*) *) read-packet))

(defun read-subpacket-count (d n)
  "Read a number of packets"
  (loop for i below n
        collecting (read-packet d)))

(defun read-subpacket-length (d n)
  (loop with end = (+ n (decode-pos d))
        until (= (decode-pos d) end)
        collecting (read-packet d)))

(defun less-op (&rest vals)
  (if (apply #'< vals) 1 0))

(defun greater-op (&rest vals)
  (if (apply #'> vals) 1 0))

(defun eq-op (&rest vals)
  (if (apply #'= vals) 1 0))

(defun kind-op (kind)
  (case kind
    (0 #'+)
    (1 #'*)
    (2 #'min)
    (3 #'max)
    ; 4 is 'literal
    (5 #'greater-op)
    (6 #'less-op)
    (7 #'eq-op)
    (otherwise 'error)))

(defun read-packet (d)
  (let ((version (to-int (read-bits d 3)))
        (kind (to-int (read-bits d 3))))
    (case kind
      (4 ;; Literal
       (let ((literals (read-literal-int d)))
         (make-packet :kind 'literal
                      :version version
                      :contents literals)
         ))

      (otherwise ;; Operator
        (let ((c (if (= 1 (read-int d 1))
                   (read-subpacket-count d (read-int d 11))
                   (read-subpacket-length d (read-int d 15))) ))
          (make-packet :kind (kind-op kind) 
                       :version version
                       :contents c)
          )))))

(defun eval-packet (pkt)
  (let ((k (packet-kind pkt)))
    (if (eq 'literal k)
      (packet-contents pkt)
      (reduce k (mapcar #'eval-packet (packet-contents pkt))))))


(defun to-packet (str)
  (read-packet (make-decode :packet (to-bits str))))

(defun count-versions (packet)
  (case (packet-kind packet)
    (literal (packet-version packet))
    (otherwise (+ (packet-version packet)
                  (reduce #'+
                          (mapcar #'count-versions
                                  (packet-contents packet)))))))


(defvar data "A20D6CE8F00033925A95338B6549C0149E3398DE75817200992531E25F005A18C8C8C0001849FDD43629C293004B001059363936796973BF3699CFF4C6C0068C9D72A1231C339802519F001029C2B9C29700B2573962930298B6B524893ABCCEC2BCD681CC010D005E104EFC7246F5EE7328C22C8400424C2538039239F720E3339940263A98029600A80021B1FE34C69100760B41C86D290A8E180256009C9639896A66533E459148200D5AC0149D4E9AACEF0F66B42696194031F000BCE7002D80A8D60277DC00B20227C807E8001CE0C00A7002DC00F300208044E000E69C00B000974C00C1003DC0089B90C1006F5E009CFC87E7E43F3FBADE77BE14C8032C9350D005662754F9BDFA32D881004B12B1964D7000B689B03254564414C016B004A6D3A6BD0DC61E2C95C6E798EA8A4600B5006EC0008542D8690B80010D89F1461B4F535296B6B305A7A4264029580021D1122146900043A0EC7884200085C598CF064C0129CFD8868024592FEE9D7692FEE9D735009E6BBECE0826842730CD250EEA49AA00C4F4B9C9D36D925195A52C4C362EB8043359AE221733DB4B14D9DCE6636ECE48132E040182D802F30AF22F131087EDD9A20804D27BEFF3FD16C8F53A5B599F4866A78D7898C0139418D00424EBB459915200C0BC01098B527C99F4EB54CF0450014A95863BDD3508038600F44C8B90A0801098F91463D1803D07634433200AB68015299EBF4CF5F27F05C600DCEBCCE3A48BC1008B1801AA0803F0CA1AC6200043A2C4558A710E364CC2D14920041E7C9A7040402E987492DE5327CF66A6A93F8CFB4BE60096006E20008543A8330780010E8931C20DCF4BFF13000A424711C4FB32999EE33351500A66E8492F185AB32091F1841C91BE2FDC53C4E80120C8C67EA7734D2448891804B2819245334372CBB0F080480E00D4C0010E82F102360803B1FA2146D963C300BA696A694A501E589A6C80")

(defun part1 (d)
  (count-versions (to-packet d)))
