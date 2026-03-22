proc normalFile {fileName} {

# new code to remove unnecessary pathname elements of the form abc/../
# from file names.  Albion Taylor, 8/15/02
#  Input:  fileName, either an absolute or relative filename
#  returns: same filename, without unnecessary xxx/../ terms.

  set fileNameList [file split $fileName]
  set fileNamePathType [file pathtype $fileName]
  switch $fileNamePathType {
    relative {
      set Head {}
       #do nothing to fileNameList
    }
    volumerelative -
    absolute {
      set Head [lindex $fileNameList 0]
      set fileNameList [lrange $fileNameList 1 end]
      if {$Head == "/" } {set fileNamePathType absolute }
    }
  }
  set outList {}
  foreach member $fileNameList {
    switch $member {
      . {
        #do nothing - self referencing directory
      }
      .. {
        switch [lindex $outList end] {
          {} -
          .. {
            lappend outList ..
          }
          default {
            set outList [lrange $outList 0 end-1]
          }
        }
      }
      default {
        lappend outList $member
      }
    }
  }
  switch $fileNamePathType {
    absolute {
      while { [lindex $outList 0] == ".." } {
        set outList [lrange $outList 1 end]
      }
    }
  }
  return [eval file join $Head $outList ]
}
