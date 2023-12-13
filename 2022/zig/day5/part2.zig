const std = @import("std");

pub fn main() !void {
    const inp = try std.fs.cwd().openFile("inp.txt", .{});
    defer inp.close();
    var buf: [10000]u8 = undefined;
    const bytes_read: usize = try inp.reader().readAll(&buf);
    const content: []u8 = buf[0..(bytes_read - 1)];
    const num_stacks = 9;

    var iter = std.mem.tokenizeSequence(u8, content, "\n\n");
    const stack_ins = iter.next().?;
    const move_ins = iter.next().?;

    var stacks: [num_stacks]std.ArrayList(u8) = undefined;
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    { // doing in block scope so that main scope doesn't get polluted
        var line_it = std.mem.tokenizeScalar(u8, stack_ins, '\n');
        var data: [8][]const u8 = undefined;
        for (0..data.len) |i| data[i] = line_it.next().?;

        var c: usize = 0;
        while (c < num_stacks) : (c += 1) {
            stacks[c] = std.ArrayList(u8).init(gpa.allocator());
            var r: usize = 0;
            while (r < 8) : (r += 1) {
                const item = data[7 - r][4 * c + 1];
                if (item == ' ') break;
                try stacks[c].append(item);
            }
        }
    }

    var num_iter = std.mem.tokenizeAny(u8, move_ins, "movefrt\n ");
    while (num_iter.next()) |num_str| {
        var how_much: u16 = try std.fmt.parseInt(u16, num_str, 10);
        const from: usize = try std.fmt.parseInt(usize, num_iter.next().?, 10);
        const to: usize = try std.fmt.parseInt(usize, num_iter.next().?, 10);
        var temp = std.ArrayList(u8).init(gpa.allocator());
        while (how_much > 0) : (how_much -= 1) {
            const item = stacks[from - 1].pop();
            try temp.append(item);
        }
        while (temp.popOrNull()) |item| {
            try stacks[to - 1].append(item);
        }
        temp.deinit();
    }
    for (stacks, 0..) |s, i| {
        std.debug.print("{}: {s}\n", .{ i, s.items });
        s.deinit();
    }
}
