let read_file (file_name: string) : string list =
    let rec read_chan (rc: in_channel): string list =
        try
            let l = input_line rc in
            let r = read_chan rc in
            l :: r
        with End_of_file -> []
    in
    let chan = open_in file_name in
    let result = read_chan chan in
    close_in chan;
    result
