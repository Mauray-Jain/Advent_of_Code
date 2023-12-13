const std = @import("std");

pub fn main() !void {
    const inp = try std.fs.cwd().openFile("inp.txt", .{});
    defer inp.close();
    const file_size: usize = (try inp.stat()).size;
    // Way 1 to read a file
    // var buf: [1024]u8 = undefined;
    // const bytes_read: usize = try inp.reader().readAll(&buf);
    // const content: []u8 = buf[0..(bytes_read - 1)];

    // Way 2:
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer if (gpa.deinit() == .leak) @panic("Mem leak\n");
    const buf: []u8 = try inp.reader().readAllAlloc(allocator, file_size);
    defer allocator.free(buf);
    var cnt: usize = 0;
    for (0..buf.len - 4) |i| {
        // if ((buf[i] == buf[i + 1]) or (buf[i] == buf[i + 2]) or (buf[i] == buf[i + 3]) or (buf[i + 1] == buf[i + 2]) or (buf[i + 1] == buf[i + 3]) or (buf[i + 2] == buf[i + 3])) {
        //     continue;
        // } else {
        //     cnt = i + 4;
        //     break;
        // }
        const slice = buf[i..(i + 4)];
        if (is_uniq(slice)) {
            cnt = i + 4;
            break;
        }
    }
    std.debug.print("{}\n", .{cnt});
}

fn is_uniq(buf: []u8) bool {
    var i: usize = 0;
    while (i < buf.len) : (i += 1) {
        var j: usize = i + 1;
        while (j < buf.len) : (j += 1) {
            if (buf[i] == buf[j]) return false;
        }
    }
    return true;
}
