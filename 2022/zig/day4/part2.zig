const std = @import("std");

pub fn main() !void {
    const inp = try std.fs.cwd().openFile("inp.txt", .{});
    defer inp.close();
    var buf: [12400]u8 = undefined;
    const bytes_read: usize = try inp.reader().readAll(&buf);
    const content = buf[0..(bytes_read - 1)];
    var line_iter = std.mem.tokenizeScalar(u8, content, '\n');
    var cnt: u16 = 0;
    while (line_iter.next()) |line| {
        var iter = std.mem.tokenize(u8, line, "-,");
        const p1l: u8 = try std.fmt.parseInt(u8, iter.next().?, 10);
        const p1r: u8 = try std.fmt.parseInt(u8, iter.next().?, 10);
        const p2l: u8 = try std.fmt.parseInt(u8, iter.next().?, 10);
        const p2r: u8 = try std.fmt.parseInt(u8, iter.next().?, 10);
        if ((p1r >= p2l) and (p2r >= p1l)) {
            cnt += 1;
        }
    }
    std.debug.print("cnt: {d}\n", .{cnt});
}
