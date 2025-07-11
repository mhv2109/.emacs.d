;;; gptel-tools-tfidf.el --- TF-IDF Implementation in Emacs Lisp. -*- lexical-binding: t -*-

;;; Commentary:
;; This file implements TF-IDF (Term Frequency-Inverse Document Frequency)
;; text vectorization from scratch in Emacs Lisp.

;;; Code:

(require 'cl-lib)

(defconst gt--tfidf-stop-words
  '("the" "is" "are" "and" "or" "but" "a" "an" "on" "over")
  "Default stop words used with GT--TFIDF-VECTORIZER.")

;; Data structure to hold our TF-IDF vectorizer
(cl-defstruct gt--tfidf-vectorizer
  "Structure to hold TF-IDF vectorizer state."
  max-features                    ; Maximum number of features to keep
  (stop-words gt--tfidf-stop-words)     ; Set of stop words to ignore
  vocabulary                  ; Hash table mapping words to indices
  idf-values                  ; Hash table mapping words to IDF values
  feature-names)   ; List of feature names in order

(defun gt--tfidf-tokenize (text &optional stop-words)
  "Tokenize TEXT by converting to lowercase, removing punctuation, and splitting.
Optionally remove STOP-WORDS."
  (let* ((lowercase-text (downcase text))
         ;; Remove punctuation and split on whitespace
         (tokens (split-string
                  (replace-regexp-in-string "[[:punct:]]" "" lowercase-text)
                  "[ \t\n\r\f]+" t)))
    ;; Remove stop words if provided
    (if stop-words
        (cl-remove-if (lambda (word) (member word stop-words)) tokens)
      tokens)))

(defun gt--tfidf-count-word-frequencies (tokens)
  "Count frequency of each word in TOKENS.
Returns a hash table mapping words to their counts."
  (let ((word-counts (make-hash-table :test 'equal)))
    (dolist (token tokens)
      (puthash token (1+ (gethash token word-counts 0)) word-counts))
    word-counts))

(defun gt--tfidf-get-unique-words (tokens)
  "Get unique words from TOKENS as a list."
  (let ((unique-words (make-hash-table :test 'equal)))
    (dolist (token tokens)
      (puthash token t unique-words))
    (hash-table-keys unique-words)))

(defun gt--tfidf-build-vocabulary (documents vectorizer)
  "Build vocabulary from DOCUMENTS for VECTORIZER.
Updates the vectorizer's vocabulary and feature-names slots."
  (let ((doc-frequency (make-hash-table :test 'equal)))

    ;; Count document frequency for each word
    (dolist (doc documents)
      (let* ((tokens (gt--tfidf-tokenize doc (gt--tfidf-vectorizer-stop-words vectorizer)))
             (unique-tokens (gt--tfidf-get-unique-words tokens)))
        (dolist (token unique-tokens)
          (puthash token (1+ (gethash token doc-frequency 0)) doc-frequency))))

    ;; Convert to list of (word . frequency) pairs and sort by frequency
    (let* ((word-freq-pairs nil)
           (max-features (gt--tfidf-vectorizer-max-features vectorizer)))

      ;; Collect all word-frequency pairs
      (maphash (lambda (word freq)
                 (push (cons word freq) word-freq-pairs))
               doc-frequency)

      ;; Sort by frequency (descending) and limit if max-features is set
      (setq word-freq-pairs
            (sort word-freq-pairs (lambda (a b) (> (cdr a) (cdr b)))))

      (when max-features
        (setq word-freq-pairs (cl-subseq word-freq-pairs 0
                                         (min max-features (length word-freq-pairs)))))

      ;; Create vocabulary hash table and feature names list
      (let ((vocabulary (make-hash-table :test 'equal))
            (feature-names nil))

        (cl-loop for (word . freq) in word-freq-pairs
                 for index from 0
                 do (progn
                      (puthash word index vocabulary)
                      (push word feature-names)))

        ;; Store in vectorizer (reverse feature-names to maintain order)
        (setf (gt--tfidf-vectorizer-vocabulary vectorizer) vocabulary)
        (setf (gt--tfidf-vectorizer-feature-names vectorizer) (reverse feature-names))))))

(defun gt--tfidf-calculate-idf (documents vectorizer)
  "Calculate IDF values for DOCUMENTS using VECTORIZER.
Updates the vectorizer's idf-values slot."
  (let ((n-documents (length documents))
        (doc-frequency (make-hash-table :test 'equal))
        (vocabulary (gt--tfidf-vectorizer-vocabulary vectorizer))
        (stop-words (gt--tfidf-vectorizer-stop-words vectorizer)))

    ;; Count document frequency for each word in vocabulary
    (dolist (doc documents)
      (let* ((tokens (gt--tfidf-tokenize doc stop-words))
             (unique-tokens (gt--tfidf-get-unique-words tokens)))
        (dolist (token unique-tokens)
          (when (gethash token vocabulary)
            (puthash token (1+ (gethash token doc-frequency 0)) doc-frequency)))))

    ;; Calculate IDF values: log(total_documents / document_frequency)
    (let ((idf-values (make-hash-table :test 'equal)))
      (maphash (lambda (word index)
                 (let ((doc-freq (gethash word doc-frequency 0)))
                   (when (> doc-freq 0)
                     (puthash word (log (/ (float n-documents) doc-freq)) idf-values))))
               vocabulary)

      ;; Store in vectorizer
      (setf (gt--tfidf-vectorizer-idf-values vectorizer) idf-values))))

(defun gt--tfidf-calculate-tf (document vectorizer)
  "Calculate TF (Term Frequency) values for DOCUMENT using VECTORIZER.
Returns a hash table mapping words to their TF values."
  (let* ((tokens (gt--tfidf-tokenize document (gt--tfidf-vectorizer-stop-words vectorizer)))
         (word-counts (gt--tfidf-count-word-frequencies tokens))
         (total-words (length tokens))
         (tf-values (make-hash-table :test 'equal))
         (vocabulary (gt--tfidf-vectorizer-vocabulary vectorizer)))

    ;; Calculate TF for each word in vocabulary
    (maphash (lambda (word index)
               (let ((count (gethash word word-counts 0)))
                 (puthash word
                          (if (> total-words 0)
                              (/ (float count) total-words)
                            0.0)
                          tf-values)))
             vocabulary)

    tf-values))

(defun gt--tfidf-fit (documents vectorizer)
  "Fit VECTORIZER to DOCUMENTS.
Builds vocabulary and calculates IDF values."
  (gt--tfidf-build-vocabulary documents vectorizer)
  (gt--tfidf-calculate-idf documents vectorizer)
  vectorizer)

(defun gt--tfidf-transform (documents vectorizer)
  "Transform DOCUMENTS to TF-IDF vectors using fitted VECTORIZER.
Returns a list of vectors (lists of numbers)."
  (let ((vectors nil)
        (feature-names (gt--tfidf-vectorizer-feature-names vectorizer))
        (idf-values (gt--tfidf-vectorizer-idf-values vectorizer)))

    (dolist (doc documents)
      (let ((tf-values (gt--tfidf-calculate-tf doc vectorizer))
            (vector nil))

        ;; Calculate TF-IDF for each feature
        (dolist (word feature-names)
          (let ((tf (gethash word tf-values 0.0))
                (idf (gethash word idf-values 0.0)))
            (push (* tf idf) vector)))

        ;; Reverse to maintain correct order
        (push (reverse vector) vectors)))

    ;; Reverse to maintain document order
    (reverse vectors)))

(defun gt--tfidf-fit-transform (documents vectorizer)
  "Fit VECTORIZER to DOCUMENTS and transform them in one step."
  (gt--tfidf-fit documents vectorizer)
  (gt--tfidf-transform documents vectorizer))

(defun tfidf-get-feature-names (vectorizer)
  "Get feature names from VECTORIZER."
  (gt--tfidf-vectorizer-feature-names vectorizer))

(defun gt--tfidf-cosine-similarity (vec1 vec2)
  "Calculate cosine similarity between VEC1 and VEC2."
  (let ((dot-product 0.0)
        (magnitude1 0.0)
        (magnitude2 0.0))

    ;; Calculate dot product and magnitudes
    (cl-loop for a in vec1
             for b in vec2
             do (progn
                  (setq dot-product (+ dot-product (* a b)))
                  (setq magnitude1 (+ magnitude1 (* a a)))
                  (setq magnitude2 (+ magnitude2 (* b b)))))

    ;; Calculate cosine similarity
    (if (or (= magnitude1 0.0) (= magnitude2 0.0))
        0.0
      (/ dot-product (* (sqrt magnitude1) (sqrt magnitude2))))))

(defun gt--tfidf-round-vector (vector decimals)
  "Round all numbers in VECTOR to DECIMALS decimal places."
  (mapcar (lambda (x) (/ (round (* x (expt 10 decimals))) (expt 10 decimals))) vector))

;; Example usage and demonstration
;; (defun tfidf-demo ()
;;   "Demonstrate TF-IDF implementation with sample documents."
;;   (interactive)

;;   ;; Sample documents
;;   (let* ((documents '("the cat sat on the mat"
;;                      "dogs are loyal animals"
;;                      "cats and dogs are pets"
;;                      "animals need food and water"
;;                      "the quick brown fox jumps over the lazy dog"))

;;          ;; Basic English stop words
;;          (stop-words '("the" "is" "are" "and" "or" "but" "a" "an" "on" "over"))

;;          ;; Create vectorizer
;;          (vectorizer (make-gt--tfidf-vectorizer
;;                      :max-features 20
;;                      :stop-words stop-words))

;;          ;; Fit and transform documents
;;          (tfidf-matrix (gt--tfidf-fit-transform documents vectorizer))

;;          ;; Get feature names
;;          (feature-names (tfidf-get-feature-names vectorizer)))

;;     ;; Display results
;;     (with-current-buffer (get-buffer-create "*TF-IDF Results*")
;;       (erase-buffer)
;;       (insert "TF-IDF Implementation Results\n")
;;       (insert "============================\n\n")

;;       ;; Show vocabulary
;;       (insert "Vocabulary: " (mapconcat 'identity feature-names ", ") "\n\n")

;;       ;; Show TF-IDF matrix
;;       (insert "TF-IDF Matrix:\n")
;;       (cl-loop for vector in tfidf-matrix
;;                for i from 1
;;                do (insert (format "Document %d: %s\n"
;;                                 i
;;                                 (mapconcat (lambda (x) (format "%.3f" x))
;;                                          vector ", "))))

;;       ;; Calculate similarity between first two documents
;;       (let ((similarity (gt--tfidf-cosine-similarity (first tfidf-matrix)
;;                                                 (second tfidf-matrix))))
;;         (insert (format "\nSimilarity between doc 1 and doc 2: %.3f\n" similarity)))

;;       ;; Show top words for first document
;;       (insert "\nTop words in document 1:\n")
;;       (let* ((doc1-vector (first tfidf-matrix))
;;              (word-scores (cl-mapcar 'cons feature-names doc1-vector))
;;              (sorted-scores (sort word-scores (lambda (a b) (> (cdr a) (cdr b))))))

;;         (cl-loop for (word . score) in sorted-scores
;;                  for count from 1 to 5
;;                  when (> score 0)
;;                  do (insert (format "  %s: %.3f\n" word score))))

;;       ;; Switch to results buffer
;;       (switch-to-buffer "*TF-IDF Results*"))))

(provide 'gptel-tools-tfidf)

;;; gptel-tools-tfidf.el ends here
