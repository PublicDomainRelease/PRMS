C     DATA  IOERR /  204 /, RLNGTH /  512,0,0,120,0 /                   DG vms
      DATA  IOERR /  204 /, RLNGTH / 2048,0,0,480,0 /                   DG ansi
C     DATA  IOERR / 2013 /, RLNGTH / 2048,0,0,480,0 /                   PC
C     DATA  IOERR /   18 /, RLNGTH / 1024,0,0,240,0 /                   PRIME
C     DATA  IOERR /    1 /, RLNGTH / 1024,0,0,240,0 /                   VAX
C
C     IOERR  - iostat from open, value returned when the file exists
C     RLNGTH - record length for file open statements
C              (1) - wdm file, record length = 2048 bytes
C              (4) - unformatted direct access file for segment q,
C                    record length = 480 bytes.
C              bytes = 2 * half_words
C                    = 4 * words
