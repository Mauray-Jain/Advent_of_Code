const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const inp = try std.fs.cwd().openFile("inp.txt", .{});
    defer inp.close();

    var buf: [1024]u8 = undefined;
    const bytes = try inp.reader().readAll(&buf);
    const content: []u8 = buf[0..bytes]; // this is a slice

    var iter = std.mem.splitScalar(u8, content, '\n'); // gives an iterator
    var sum: u16 = 0;
    var max_sum: u16 = 0;
    while (true) {
        var num = iter.next() orelse break;
        if (num.len != 0) {
            var num_int: u16 = try std.fmt.parseInt(u16, num, 10);
            sum += num_int;
        } else {
            max_sum = if (max_sum > sum) max_sum else sum;
            sum = 0;
        }
    }
    try stdout.print("{}\n", .{max_sum});
}
