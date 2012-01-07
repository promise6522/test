
a.out:     file format elf64-x86-64


Disassembly of section .init:

00000000004003c8 <_init>:
  4003c8:	48 83 ec 08          	sub    $0x8,%rsp
  4003cc:	e8 6b 00 00 00       	callq  40043c <call_gmon_start>
  4003d1:	e8 fa 00 00 00       	callq  4004d0 <frame_dummy>
  4003d6:	e8 45 02 00 00       	callq  400620 <__do_global_ctors_aux>
  4003db:	48 83 c4 08          	add    $0x8,%rsp
  4003df:	c3                   	retq   

Disassembly of section .plt:

00000000004003e0 <printf@plt-0x10>:
  4003e0:	ff 35 0a 0c 20 00    	pushq  0x200c0a(%rip)        # 600ff0 <_GLOBAL_OFFSET_TABLE_+0x8>
  4003e6:	ff 25 0c 0c 20 00    	jmpq   *0x200c0c(%rip)        # 600ff8 <_GLOBAL_OFFSET_TABLE_+0x10>
  4003ec:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004003f0 <printf@plt>:
  4003f0:	ff 25 0a 0c 20 00    	jmpq   *0x200c0a(%rip)        # 601000 <_GLOBAL_OFFSET_TABLE_+0x18>
  4003f6:	68 00 00 00 00       	pushq  $0x0
  4003fb:	e9 e0 ff ff ff       	jmpq   4003e0 <_init+0x18>

0000000000400400 <__libc_start_main@plt>:
  400400:	ff 25 02 0c 20 00    	jmpq   *0x200c02(%rip)        # 601008 <_GLOBAL_OFFSET_TABLE_+0x20>
  400406:	68 01 00 00 00       	pushq  $0x1
  40040b:	e9 d0 ff ff ff       	jmpq   4003e0 <_init+0x18>

Disassembly of section .text:

0000000000400410 <_start>:
  400410:	31 ed                	xor    %ebp,%ebp
  400412:	49 89 d1             	mov    %rdx,%r9
  400415:	5e                   	pop    %rsi
  400416:	48 89 e2             	mov    %rsp,%rdx
  400419:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  40041d:	50                   	push   %rax
  40041e:	54                   	push   %rsp
  40041f:	49 c7 c0 10 06 40 00 	mov    $0x400610,%r8
  400426:	48 c7 c1 80 05 40 00 	mov    $0x400580,%rcx
  40042d:	48 c7 c7 2e 05 40 00 	mov    $0x40052e,%rdi
  400434:	e8 c7 ff ff ff       	callq  400400 <__libc_start_main@plt>
  400439:	f4                   	hlt    
  40043a:	90                   	nop
  40043b:	90                   	nop

000000000040043c <call_gmon_start>:
  40043c:	48 83 ec 08          	sub    $0x8,%rsp
  400440:	48 8b 05 99 0b 20 00 	mov    0x200b99(%rip),%rax        # 600fe0 <_DYNAMIC+0x190>
  400447:	48 85 c0             	test   %rax,%rax
  40044a:	74 02                	je     40044e <call_gmon_start+0x12>
  40044c:	ff d0                	callq  *%rax
  40044e:	48 83 c4 08          	add    $0x8,%rsp
  400452:	c3                   	retq   
  400453:	90                   	nop
  400454:	90                   	nop
  400455:	90                   	nop
  400456:	90                   	nop
  400457:	90                   	nop
  400458:	90                   	nop
  400459:	90                   	nop
  40045a:	90                   	nop
  40045b:	90                   	nop
  40045c:	90                   	nop
  40045d:	90                   	nop
  40045e:	90                   	nop
  40045f:	90                   	nop

0000000000400460 <__do_global_dtors_aux>:
  400460:	55                   	push   %rbp
  400461:	48 89 e5             	mov    %rsp,%rbp
  400464:	53                   	push   %rbx
  400465:	48 83 ec 08          	sub    $0x8,%rsp
  400469:	80 3d b0 0b 20 00 00 	cmpb   $0x0,0x200bb0(%rip)        # 601020 <__bss_start>
  400470:	75 4b                	jne    4004bd <__do_global_dtors_aux+0x5d>
  400472:	bb 40 0e 60 00       	mov    $0x600e40,%ebx
  400477:	48 8b 05 aa 0b 20 00 	mov    0x200baa(%rip),%rax        # 601028 <dtor_idx.6559>
  40047e:	48 81 eb 38 0e 60 00 	sub    $0x600e38,%rbx
  400485:	48 c1 fb 03          	sar    $0x3,%rbx
  400489:	48 83 eb 01          	sub    $0x1,%rbx
  40048d:	48 39 d8             	cmp    %rbx,%rax
  400490:	73 24                	jae    4004b6 <__do_global_dtors_aux+0x56>
  400492:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
  400498:	48 83 c0 01          	add    $0x1,%rax
  40049c:	48 89 05 85 0b 20 00 	mov    %rax,0x200b85(%rip)        # 601028 <dtor_idx.6559>
  4004a3:	ff 14 c5 38 0e 60 00 	callq  *0x600e38(,%rax,8)
  4004aa:	48 8b 05 77 0b 20 00 	mov    0x200b77(%rip),%rax        # 601028 <dtor_idx.6559>
  4004b1:	48 39 d8             	cmp    %rbx,%rax
  4004b4:	72 e2                	jb     400498 <__do_global_dtors_aux+0x38>
  4004b6:	c6 05 63 0b 20 00 01 	movb   $0x1,0x200b63(%rip)        # 601020 <__bss_start>
  4004bd:	48 83 c4 08          	add    $0x8,%rsp
  4004c1:	5b                   	pop    %rbx
  4004c2:	c9                   	leaveq 
  4004c3:	c3                   	retq   
  4004c4:	66 66 66 2e 0f 1f 84 	data32 data32 nopw %cs:0x0(%rax,%rax,1)
  4004cb:	00 00 00 00 00 

00000000004004d0 <frame_dummy>:
  4004d0:	48 83 3d 70 09 20 00 	cmpq   $0x0,0x200970(%rip)        # 600e48 <__JCR_END__>
  4004d7:	00 
  4004d8:	55                   	push   %rbp
  4004d9:	48 89 e5             	mov    %rsp,%rbp
  4004dc:	74 12                	je     4004f0 <frame_dummy+0x20>
  4004de:	b8 00 00 00 00       	mov    $0x0,%eax
  4004e3:	48 85 c0             	test   %rax,%rax
  4004e6:	74 08                	je     4004f0 <frame_dummy+0x20>
  4004e8:	bf 48 0e 60 00       	mov    $0x600e48,%edi
  4004ed:	c9                   	leaveq 
  4004ee:	ff e0                	jmpq   *%rax
  4004f0:	c9                   	leaveq 
  4004f1:	c3                   	retq   
  4004f2:	90                   	nop
  4004f3:	90                   	nop

00000000004004f4 <multi_add>:
  4004f4:	55                   	push   %rbp
  4004f5:	48 89 e5             	mov    %rsp,%rbp
  4004f8:	48 89 7d f8          	mov    %rdi,-0x8(%rbp)
  4004fc:	48 89 75 f0          	mov    %rsi,-0x10(%rbp)
  400500:	48 89 55 e8          	mov    %rdx,-0x18(%rbp)
  400504:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  400508:	8b 10                	mov    (%rax),%edx
  40050a:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  40050e:	8b 00                	mov    (%rax),%eax
  400510:	01 c2                	add    %eax,%edx
  400512:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  400516:	89 10                	mov    %edx,(%rax)
  400518:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  40051c:	8b 10                	mov    (%rax),%edx
  40051e:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  400522:	8b 00                	mov    (%rax),%eax
  400524:	01 c2                	add    %eax,%edx
  400526:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  40052a:	89 10                	mov    %edx,(%rax)
  40052c:	c9                   	leaveq 
  40052d:	c3                   	retq   

000000000040052e <main>:
  40052e:	55                   	push   %rbp
  40052f:	48 89 e5             	mov    %rsp,%rbp
  400532:	48 83 ec 10          	sub    $0x10,%rsp
  400536:	c7 45 fc 01 00 00 00 	movl   $0x1,-0x4(%rbp)
  40053d:	c7 45 f8 02 00 00 00 	movl   $0x2,-0x8(%rbp)
  400544:	c7 45 f4 01 00 00 00 	movl   $0x1,-0xc(%rbp)
  40054b:	48 8d 55 f4          	lea    -0xc(%rbp),%rdx
  40054f:	48 8d 4d f8          	lea    -0x8(%rbp),%rcx
  400553:	48 8d 45 fc          	lea    -0x4(%rbp),%rax
  400557:	48 89 ce             	mov    %rcx,%rsi
  40055a:	48 89 c7             	mov    %rax,%rdi
  40055d:	e8 92 ff ff ff       	callq  4004f4 <multi_add>
  400562:	8b 55 f8             	mov    -0x8(%rbp),%edx
  400565:	8b 4d fc             	mov    -0x4(%rbp),%ecx
  400568:	b8 6c 06 40 00       	mov    $0x40066c,%eax
  40056d:	89 ce                	mov    %ecx,%esi
  40056f:	48 89 c7             	mov    %rax,%rdi
  400572:	b8 00 00 00 00       	mov    $0x0,%eax
  400577:	e8 74 fe ff ff       	callq  4003f0 <printf@plt>
  40057c:	c9                   	leaveq 
  40057d:	c3                   	retq   
  40057e:	90                   	nop
  40057f:	90                   	nop

0000000000400580 <__libc_csu_init>:
  400580:	48 89 6c 24 d8       	mov    %rbp,-0x28(%rsp)
  400585:	4c 89 64 24 e0       	mov    %r12,-0x20(%rsp)
  40058a:	48 8d 2d 93 08 20 00 	lea    0x200893(%rip),%rbp        # 600e24 <__init_array_end>
  400591:	4c 8d 25 8c 08 20 00 	lea    0x20088c(%rip),%r12        # 600e24 <__init_array_end>
  400598:	4c 89 6c 24 e8       	mov    %r13,-0x18(%rsp)
  40059d:	4c 89 74 24 f0       	mov    %r14,-0x10(%rsp)
  4005a2:	4c 89 7c 24 f8       	mov    %r15,-0x8(%rsp)
  4005a7:	48 89 5c 24 d0       	mov    %rbx,-0x30(%rsp)
  4005ac:	48 83 ec 38          	sub    $0x38,%rsp
  4005b0:	4c 29 e5             	sub    %r12,%rbp
  4005b3:	41 89 fd             	mov    %edi,%r13d
  4005b6:	49 89 f6             	mov    %rsi,%r14
  4005b9:	48 c1 fd 03          	sar    $0x3,%rbp
  4005bd:	49 89 d7             	mov    %rdx,%r15
  4005c0:	e8 03 fe ff ff       	callq  4003c8 <_init>
  4005c5:	48 85 ed             	test   %rbp,%rbp
  4005c8:	74 1c                	je     4005e6 <__libc_csu_init+0x66>
  4005ca:	31 db                	xor    %ebx,%ebx
  4005cc:	0f 1f 40 00          	nopl   0x0(%rax)
  4005d0:	4c 89 fa             	mov    %r15,%rdx
  4005d3:	4c 89 f6             	mov    %r14,%rsi
  4005d6:	44 89 ef             	mov    %r13d,%edi
  4005d9:	41 ff 14 dc          	callq  *(%r12,%rbx,8)
  4005dd:	48 83 c3 01          	add    $0x1,%rbx
  4005e1:	48 39 eb             	cmp    %rbp,%rbx
  4005e4:	72 ea                	jb     4005d0 <__libc_csu_init+0x50>
  4005e6:	48 8b 5c 24 08       	mov    0x8(%rsp),%rbx
  4005eb:	48 8b 6c 24 10       	mov    0x10(%rsp),%rbp
  4005f0:	4c 8b 64 24 18       	mov    0x18(%rsp),%r12
  4005f5:	4c 8b 6c 24 20       	mov    0x20(%rsp),%r13
  4005fa:	4c 8b 74 24 28       	mov    0x28(%rsp),%r14
  4005ff:	4c 8b 7c 24 30       	mov    0x30(%rsp),%r15
  400604:	48 83 c4 38          	add    $0x38,%rsp
  400608:	c3                   	retq   
  400609:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000400610 <__libc_csu_fini>:
  400610:	f3 c3                	repz retq 
  400612:	90                   	nop
  400613:	90                   	nop
  400614:	90                   	nop
  400615:	90                   	nop
  400616:	90                   	nop
  400617:	90                   	nop
  400618:	90                   	nop
  400619:	90                   	nop
  40061a:	90                   	nop
  40061b:	90                   	nop
  40061c:	90                   	nop
  40061d:	90                   	nop
  40061e:	90                   	nop
  40061f:	90                   	nop

0000000000400620 <__do_global_ctors_aux>:
  400620:	55                   	push   %rbp
  400621:	48 89 e5             	mov    %rsp,%rbp
  400624:	53                   	push   %rbx
  400625:	48 83 ec 08          	sub    $0x8,%rsp
  400629:	48 8b 05 f8 07 20 00 	mov    0x2007f8(%rip),%rax        # 600e28 <__CTOR_LIST__>
  400630:	48 83 f8 ff          	cmp    $0xffffffffffffffff,%rax
  400634:	74 19                	je     40064f <__do_global_ctors_aux+0x2f>
  400636:	bb 28 0e 60 00       	mov    $0x600e28,%ebx
  40063b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
  400640:	48 83 eb 08          	sub    $0x8,%rbx
  400644:	ff d0                	callq  *%rax
  400646:	48 8b 03             	mov    (%rbx),%rax
  400649:	48 83 f8 ff          	cmp    $0xffffffffffffffff,%rax
  40064d:	75 f1                	jne    400640 <__do_global_ctors_aux+0x20>
  40064f:	48 83 c4 08          	add    $0x8,%rsp
  400653:	5b                   	pop    %rbx
  400654:	c9                   	leaveq 
  400655:	c3                   	retq   
  400656:	90                   	nop
  400657:	90                   	nop

Disassembly of section .fini:

0000000000400658 <_fini>:
  400658:	48 83 ec 08          	sub    $0x8,%rsp
  40065c:	e8 ff fd ff ff       	callq  400460 <__do_global_dtors_aux>
  400661:	48 83 c4 08          	add    $0x8,%rsp
  400665:	c3                   	retq   
