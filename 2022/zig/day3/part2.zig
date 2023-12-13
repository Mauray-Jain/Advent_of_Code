const std = @import("std");

pub fn main() !void {
    const inp = try std.fs.cwd().openFile("inp.txt", .{});
    defer inp.close();
    var buf: [10000]u8 = undefined;
    const bytes_read = try inp.reader().readAll(&buf);
    var it = std.mem.tokenizeScalar(u8, buf[0..(bytes_read - 1)], '\n');
    var sum: u16 = 0;
    while (it.next()) |line1| {
        const line2 = it.next().?;
        const line3 = it.next().?;
        const shared: u8 = for (line1) |char| {
            if (std.mem.indexOfScalar(u8, line2, char) != null and std.mem.indexOfScalar(u8, line3, char) != null) break char else continue;
        } else unreachable;
        sum += switch (shared) {
            'a'...'z' => shared - 'a' + 1,
            'A'...'Z' => shared - 'A' + 27,
            else => unreachable,
        };
    }
    std.debug.print("{d}\n", .{sum});
}
