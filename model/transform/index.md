# Topology transformations

Beware cycles!

| Tag        | Before                  | Begin              | End                | After                       | Finally                           | Force Exit       |
|------------|-------------------------|--------------------|--------------------|-----------------------------|-----------------------------------|------------------|
| Default    | parent.begin OR _path_  | _path_             | **<-** begin       | **<-** ends                 | force ? **<-** after: **<-** ends | _false_          | 
| seq        | -                       | -                  | lastChild.finally  | -                           | -                                 | -                |
| seq/*      | prev.finally OR default | -                  | -                  | next.begin OR parent.after  | -                                 | -                | 
| xor/*:0    | -                       | -                  | -                  | parent.after                | -                                 | hasExecLater     |
| xor/*:1    | prev.ends               | -                  | -                  | parent.after                | -                                 | hasExecLater     | 
| xor        | -                       | -                  | lastChild.finally  | -                           | -                                 | -                |
| par/*      | -                       | -                  | **<-** before      | parent.after                | -                              | exportsUsedLater | 
| for        | -                       | fc.begins(until i) | -                  | -                           | -                                 | -                | 
| noExec     | -                       | -                  | **<-** begin       | -                           | -                              | -                | 


