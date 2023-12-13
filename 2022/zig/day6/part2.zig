const std = @import("std");

pub fn main() !void {
    const inp = try std.fs.cwd().openFile("inp.txt", .{});
    defer inp.close();
    const file_size: usize = (try inp.stat()).size;
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer if (gpa.deinit() == .leak) @panic("Mem leak\n");
    const buf: []u8 = try inp.reader().readAllAlloc(allocator, file_size);
    defer allocator.free(buf);
    var cnt: usize = 0;
    for (0..buf.len - 14) |i| {
        const slice = buf[i..(i + 14)];
        if (is_uniq(slice)) {
            cnt = i + 14;
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
