# Chordy

## Look up

1. `successor(k)` 表示环上从 k 顺时针起（包括 k 本身）第一个遇到的 node.

2. 每个 node 维护长度为 $m$ 的表 `Finger Table`.
3. `Finger Table` 中第 $i$ 项储存了在 $(n + 2^{i-1}\space mod \space 2^{m})$ 之后环上第一个 node $s$, 即 $s = successor(n + 2^{i-1})$.

4. node $n$ 在 `find_successor()` 时, 会与一系列逐渐靠近 identifier k 的 node 进行 RPC. 如果其中有某个 node $n'$ 使得$k\in(n',n'.successor]$ , 那么 $n'$ 就是 k 的 predecessor.
5. 一种直观上的理解是, `find_predecessor()` 每次迭代时，node $n'$ 离 identifier k 的距离都会减半. 因此使用该算法进行 lookup 的平均时间复杂度为 $\mathcal{\frac{1}{2}O(logN)}$.

## Add node

1. `predecessor` 环上沿逆时针方向走遇到的第一个 node.

2. ```python
   # 修改后的join，功能不变
   # 添加node n到系统中。可能会用到已有node n'来进行初始化
   def n.join(n'):
     predecessor = None
     successor = n'.find_successor(n)
   
   # 在后台定期运行
   def n.stabilize():
     x = successor.predecessor  # 1
     if x in (n, successor):    # 2
       successor = x            # 3
     successor.notify(n)        # 4
   
   # n'有可能是n最新的predecessor
   def n.notify(n'):
     if (predecessor is None) or (n' in (predecessor, n)):  # 5
       predecessor = n'                                     # 6
   ```

   假如 node $n$ 被加入到了node $n_{p}$ 和 node $n_{s}$ 之间, 即环在顺时针方向上由 ${..., n_{p}, n_{s}, ...}$ 变为 ${..., n_{p}, n, n_{s}, ...}$.

   * `n.join()`: 执行后 $n_p$, $n_s$ 没有任何变化。`n.predecessor = None, n.successor = ns`.

   * 由于 $n_p$, $n_s$ 没有变化, 它们执行 `stabilize()` 时也不会检测到 $n$。而 $n$ 执行 `n.stabilize()` 时,  `#1` 返回的是 $n_p$. 因为`#2` $n_p \notin (n, n_s)$, `#3`被跳过. 而`#4`被执行, 即 node $n$ 告知 node $n_s$ 自己的存在.

   * 当 `ns.notify(n)` 执行时, 由于 `#5` $n\in (n_p, n_s)$ , `#6 `被执行, 即 $n_s$ 的 predecessor 被更新为 $n$. 

   * 当 $n_p$ 再次执行 `np.stabilize()` 时，它会发现 `#1` 返回的是 n. 由于`#2` $n\in(n_p, n_s)$, 它将执行 `#3` , 将 successor 更新为n。最后 `#4` 会通知 node $n$ 关于 node $n_p$ 的存在.

   * 当 `n.notify(np)` 执行时，由于 predecessor 仍然为 None, node $n$会将 predecessor 设为 $n_p$.